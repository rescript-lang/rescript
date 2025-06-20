/* **
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String)

let cmtCheckAnnotations = (~checkAnnotation, inputCMT) =>
  switch inputCMT.Cmt_format.cmt_annots {
  | Implementation(structure) => structure->Annotation.structureCheckAnnotation(~checkAnnotation)
  | Interface(signature) => signature->Annotation.signatureCheckAnnotation(~checkAnnotation)
  | _ => false
  }

let cmtHasTypeErrors = inputCMT =>
  switch inputCMT.Cmt_format.cmt_annots {
  | Partial_implementation(_)
  | Partial_interface(_) => true
  | _ => false
  }

let structureItemIsDeclaration = structItem =>
  switch structItem.Typedtree.str_desc {
  | Typedtree.Tstr_type(_)
  | Tstr_modtype(_)
  | Tstr_module(_) => true
  | _ => false
  }

let signatureItemIsDeclaration = signatureItem =>
  switch signatureItem.Typedtree.sig_desc {
  | Typedtree.Tsig_type(_)
  | Tsig_modtype(_) => true
  | _ => false
  }

let inputCmtTranslateTypeDeclarations = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  inputCMT,
): CodeItem.translation => {
  let {Cmt_format.cmt_annots: cmt_annots} = inputCMT
  let typeEnv = TypeEnv.root()
  let translations = switch cmt_annots {
  | Implementation(structure) =>
    {
      ...structure,
      str_items: structure.str_items->List.filter(structureItemIsDeclaration),
    }->TranslateStructure.translateStructure(~config, ~outputFileRelative, ~resolver, ~typeEnv)

  | Interface(signature) =>
    {
      ...signature,
      sig_items: signature.sig_items->List.filter(signatureItemIsDeclaration),
    }->TranslateSignature.translateSignature(~config, ~outputFileRelative, ~resolver, ~typeEnv)

  | Packed(_)
  | Partial_implementation(_)
  | Partial_interface(_) => list{}
  }
  translations
  ->Translation.combine
  ->Translation.addTypeDeclarationsFromModuleEquations(~typeEnv)
}

let translateCMT = (~config, ~outputFileRelative, ~resolver, inputCMT): Translation.t => {
  let {Cmt_format.cmt_annots: cmt_annots} = inputCMT
  let typeEnv = TypeEnv.root()
  let translations = switch cmt_annots {
  | Implementation(structure) =>
    structure->TranslateStructure.translateStructure(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
    )
  | Interface(signature) =>
    signature->TranslateSignature.translateSignature(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
    )
  | _ => list{}
  }
  translations
  ->Translation.combine
  ->Translation.addTypeDeclarationsFromModuleEquations(~typeEnv)
}

let emitTranslation = (
  ~config,
  ~fileName,
  ~isInterface,
  ~outputFile,
  ~outputFileRelative,
  ~resolver,
  ~signFile,
  translation,
) => {
  let codeText =
    translation->EmitJs.emitTranslationAsString(
      ~config,
      ~fileName,
      ~outputFileRelative,
      ~resolver,
      ~inputCmtTranslateTypeDeclarations,
    )
  let fileContents = signFile(
    EmitType.fileHeader(
      ~config,
      ~sourceFile=(fileName->ModuleName.toString) ++ (isInterface ? ".rei" : ".re"),
    ) ++
    ("\n" ++
    (codeText ++ "\n")),
  )

  GeneratedFiles.writeFileIfRequired(~outputFile, ~fileContents)
}

let readCmt = cmtFile =>
  try Cmt_format.read_cmt(cmtFile) catch {
  | Cmi_format.Error(_) =>
    Log_.item("Error loading %s\n\n", cmtFile)
    Log_.item(
      "It looks like you might be using an old version of Bucklescript, or have stale compilation artifacts.\n",
    )
    Log_.item("Check that bs-platform is version 6.2.x or later.\n")
    Log_.item("And try to clean and rebuild.\n\n")
    assert(false)
  }

let processCmtFile = (~signFile, ~config, cmt) => {
  let cmtFile = cmt->Paths.getCmtFile
  if cmtFile != "" {
    let outputFile = cmt->Paths.getOutputFile(~config)
    let outputFileRelative = cmt->Paths.getOutputFileRelative(~config)
    let fileName = cmt->Paths.getModuleName
    let isInterface = Filename.check_suffix(cmtFile, ".cmti")
    let resolver = ModuleResolver.createLazyResolver(
      ~config,
      ~extensions=list{".re", EmitType.shimExtension(~config)},
      ~excludeFile=fname => fname == "React.re" || fname == "ReasonReact.re",
    )
    let (inputCMT, hasGenTypeAnnotations) = {
      let inputCMT = readCmt(cmtFile)
      let ignoreInterface = ref(false)
      let checkAnnotation = (~loc as _, attributes) => {
        if (
          attributes
          ->Annotation.getAttributePayload(Annotation.tagIsGenTypeIgnoreInterface) != None
        ) {
          ignoreInterface := true
        }
        attributes
        ->Annotation.getAttributePayload(Annotation.tagIsOneOfTheGenTypeAnnotations) != None
      }

      let hasGenTypeAnnotations = inputCMT->cmtCheckAnnotations(~checkAnnotation)
      if isInterface {
        let cmtFileImpl = (cmtFile->Filename.chop_extension) ++ ".cmt"
        let inputCMTImpl = readCmt(cmtFileImpl)
        let hasGenTypeAnnotationsImpl = inputCMTImpl->cmtCheckAnnotations(~checkAnnotation=(
          ~loc,
          attributes,
        ) =>
          if attributes->checkAnnotation(~loc) {
            if !ignoreInterface.contents {
              Log_.Color.setup()
              Log_.info(~loc, ~name="Warning genType", (ppf, ()) =>
                Format.fprintf(ppf, "Annotation is ignored as there's a .rei file")
              )
            }
            true
          } else {
            false
          }
        )
        (
          ignoreInterface.contents ? inputCMTImpl : inputCMT,
          ignoreInterface.contents ? hasGenTypeAnnotationsImpl : hasGenTypeAnnotations,
        )
      } else {
        (inputCMT, hasGenTypeAnnotations)
      }
    }
    if hasGenTypeAnnotations {
      inputCMT
      ->translateCMT(~config, ~outputFileRelative, ~resolver)
      ->emitTranslation(
        ~config,
        ~fileName,
        ~isInterface,
        ~outputFile,
        ~outputFileRelative,
        ~resolver,
        ~signFile,
      )
    } else if inputCMT->cmtHasTypeErrors {
      outputFile->GeneratedFiles.logFileAction(TypeError)
    } else {
      outputFile->GeneratedFiles.logFileAction(NoMatch)
      if Sys.file_exists(outputFile) {
        Unix.unlink(outputFile)
      }
    }
  }
}
