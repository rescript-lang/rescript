'use strict';


var _map = {"variant0":"variant0","variant1":"variant1","variant2":"variant2","variant3":"variant3","variant4":"variant4","variant5":"variant5","variant6":"variant6","variant7":"variant7","variant8":"variant8","variant9":"variant9","variant10":"variant10","variant11":"variant11","variant12":"variant12","variant13":"variant13","variant14":"variant14","variant15":"variant15","variant16":"variant16","variant17":"variant17","variant18":"variant18","variant19":"variant19","variant20":"variant20","variant21":"variant21","variant22":"variant22","variant23":"variant23","variant24":"variant24","variant25":"variant25","variant26":"variant26","variant27":"variant27","variant28":"variant28","variant29":"variant29","variant30":"variant30","variant31":"variant31","variant32":"variant32","variant33":"variant33","variant34":"variant34","variant35":"variant35","variant36":"variant36","variant37":"variant37","variant38":"variant38","variant39":"variant39","variant40":"variant40","variant41":"variant41","variant42":"variant42","variant43":"variant43","variant44":"variant44","variant45":"variant45","variant46":"variant46","variant47":"variant47","variant48":"variant48","variant49":"variant49","variant50":"variant50","variant51":"variant51","variant52":"variant52","variant53":"variant53","variant54":"variant54","variant55":"variant55","variant56":"variant56","variant57":"variant57","variant58":"variant58","variant59":"variant59","variant60":"variant60","variant61":"variant61","variant62":"variant62","variant63":"variant63","variant64":"variant64","variant65":"variant65","variant66":"variant66","variant67":"variant67","variant68":"variant68","variant69":"variant69","variant70":"variant70","variant71":"variant71","variant72":"variant72","variant73":"variant73","variant74":"variant74","variant75":"variant75","variant76":"variant76","variant77":"variant77","variant78":"variant78","variant79":"variant79","variant80":"variant80","variant81":"variant81","variant82":"variant82","variant83":"variant83","variant84":"variant84","variant85":"variant85","variant86":"variant86","variant87":"variant87","variant88":"variant88","variant89":"variant89","variant90":"variant90","variant91":"variant91","variant92":"variant92","variant93":"variant93","variant94":"variant94","variant95":"variant95","variant96":"variant96","variant97":"variant97","variant98":"variant98","variant99":"variant99","variant100":"variant100","variant101":"variant101","variant102":"variant102","variant103":"variant103","variant104":"variant104","variant105":"variant105","variant106":"variant106","variant107":"variant107","variant108":"variant108","variant109":"variant109","variant110":"variant110","variant111":"variant111","variant112":"variant112","variant113":"variant113","variant114":"variant114","variant115":"variant115","variant116":"variant116","variant117":"variant117","variant118":"variant118","variant119":"variant119","variant120":"variant120","variant121":"variant121","variant122":"variant122","variant123":"variant123","variant124":"variant124","variant125":"variant125","variant126":"variant126","variant127":"variant127","variant128":"variant128","variant129":"variant129","variant130":"variant130","variant131":"variant131","variant132":"variant132","variant133":"variant133","variant134":"variant134","variant135":"variant135","variant136":"variant136","variant137":"variant137","variant138":"variant138","variant139":"variant139","variant140":"variant140","variant141":"variant141","variant142":"variant142","variant143":"variant143","variant144":"variant144","variant145":"variant145","variant146":"variant146","variant147":"variant147","variant148":"variant148","variant149":"variant149","variant150":"variant150","variant151":"variant151","variant152":"variant152","variant153":"variant153","variant154":"variant154","variant155":"variant155","variant156":"variant156","variant157":"variant157","variant158":"variant158","variant159":"variant159","variant160":"variant160","variant161":"variant161","variant162":"variant162","variant163":"variant163","variant164":"variant164","variant165":"variant165","variant166":"variant166","variant167":"variant167","variant168":"variant168","variant169":"variant169","variant170":"variant170","variant171":"variant171","variant172":"variant172","variant173":"variant173","variant174":"variant174","variant175":"variant175","variant176":"variant176","variant177":"variant177","variant178":"variant178","variant179":"variant179","variant180":"variant180","variant181":"variant181","variant182":"variant182","variant183":"variant183","variant184":"variant184","variant185":"variant185","variant186":"variant186","variant187":"variant187","variant188":"variant188","variant189":"variant189","variant190":"variant190","variant191":"variant191","variant192":"variant192","variant193":"variant193","variant194":"variant194","variant195":"variant195","variant196":"variant196","variant197":"variant197","variant198":"variant198","variant199":"variant199","variant200":"variant200","variant201":"variant201","variant202":"variant202","variant203":"variant203","variant204":"variant204","variant205":"variant205","variant206":"variant206","variant207":"variant207","variant208":"variant208","variant209":"variant209","variant210":"variant210","variant211":"variant211","variant212":"variant212","variant213":"variant213","variant214":"variant214","variant215":"variant215","variant216":"variant216","variant217":"variant217","variant218":"variant218","variant219":"variant219","variant220":"variant220","variant221":"variant221","variant222":"variant222","variant223":"variant223","variant224":"variant224","variant225":"variant225","variant226":"variant226","variant227":"variant227","variant228":"variant228","variant229":"variant229","variant230":"variant230","variant231":"variant231","variant232":"variant232","variant233":"variant233","variant234":"variant234","variant235":"variant235","variant236":"variant236","variant237":"variant237","variant238":"variant238","variant239":"variant239","variant240":"variant240","variant241":"variant241","variant242":"variant242","variant243":"variant243","variant244":"variant244","variant245":"variant245","variant246":"variant246","variant247":"variant247","variant248":"variant248","variant249":"variant249","variant250":"variant250","variant251":"variant251","variant252":"variant252","variant253":"variant253","variant254":"variant254","variant255":"variant255","variant256":"variant256","variant257":"variant257","variant258":"variant258","variant259":"variant259","variant260":"variant260","variant261":"variant261","variant262":"variant262","variant263":"variant263","variant264":"variant264","variant265":"variant265","variant266":"variant266","variant267":"variant267","variant268":"variant268","variant269":"variant269","variant270":"variant270","variant271":"variant271","variant272":"variant272","variant273":"variant273","variant274":"variant274","variant275":"variant275","variant276":"variant276","variant277":"variant277","variant278":"variant278","variant279":"variant279","variant280":"variant280","variant281":"variant281","variant282":"variant282","variant283":"variant283","variant284":"variant284","variant285":"variant285","variant286":"variant286","variant287":"variant287","variant288":"variant288","variant289":"variant289","variant290":"variant290","variant291":"variant291","variant292":"variant292","variant293":"variant293","variant294":"variant294","variant295":"variant295","variant296":"variant296","variant297":"variant297","variant298":"variant298","variant299":"variant299"};

function tToJs(param) {
  return param;
}

function tFromJs(param) {
  return _map[param];
}

function eq(x, y) {
  if (x !== undefined) {
    if (y !== undefined) {
      return x === y;
    } else {
      return false;
    }
  } else {
    return y === undefined;
  }
}

if ("variant0" !== "variant0") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          314,
          0
        ],
        Error: new Error()
      };
}

if ("variant1" !== "variant1") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          315,
          0
        ],
        Error: new Error()
      };
}

if ("variant2" !== "variant2") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          316,
          0
        ],
        Error: new Error()
      };
}

if ("variant3" !== "variant3") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          317,
          0
        ],
        Error: new Error()
      };
}

if ("variant4" !== "variant4") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          318,
          0
        ],
        Error: new Error()
      };
}

if ("variant5" !== "variant5") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          319,
          0
        ],
        Error: new Error()
      };
}

if ("variant6" !== "variant6") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          320,
          0
        ],
        Error: new Error()
      };
}

if ("variant7" !== "variant7") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          321,
          0
        ],
        Error: new Error()
      };
}

if ("variant8" !== "variant8") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          322,
          0
        ],
        Error: new Error()
      };
}

if ("variant9" !== "variant9") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          323,
          0
        ],
        Error: new Error()
      };
}

if ("variant10" !== "variant10") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          324,
          0
        ],
        Error: new Error()
      };
}

if ("variant11" !== "variant11") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          325,
          0
        ],
        Error: new Error()
      };
}

if ("variant12" !== "variant12") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          326,
          0
        ],
        Error: new Error()
      };
}

if ("variant13" !== "variant13") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          327,
          0
        ],
        Error: new Error()
      };
}

if ("variant14" !== "variant14") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          328,
          0
        ],
        Error: new Error()
      };
}

if ("variant15" !== "variant15") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          329,
          0
        ],
        Error: new Error()
      };
}

if ("variant16" !== "variant16") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          330,
          0
        ],
        Error: new Error()
      };
}

if ("variant17" !== "variant17") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          331,
          0
        ],
        Error: new Error()
      };
}

if ("variant18" !== "variant18") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          332,
          0
        ],
        Error: new Error()
      };
}

if ("variant19" !== "variant19") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          333,
          0
        ],
        Error: new Error()
      };
}

if ("variant20" !== "variant20") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          334,
          0
        ],
        Error: new Error()
      };
}

if ("variant21" !== "variant21") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          335,
          0
        ],
        Error: new Error()
      };
}

if ("variant22" !== "variant22") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          336,
          0
        ],
        Error: new Error()
      };
}

if ("variant23" !== "variant23") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          337,
          0
        ],
        Error: new Error()
      };
}

if ("variant24" !== "variant24") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          338,
          0
        ],
        Error: new Error()
      };
}

if ("variant25" !== "variant25") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          339,
          0
        ],
        Error: new Error()
      };
}

if ("variant26" !== "variant26") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          340,
          0
        ],
        Error: new Error()
      };
}

if ("variant27" !== "variant27") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          341,
          0
        ],
        Error: new Error()
      };
}

if ("variant28" !== "variant28") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          342,
          0
        ],
        Error: new Error()
      };
}

if ("variant29" !== "variant29") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          343,
          0
        ],
        Error: new Error()
      };
}

if ("variant30" !== "variant30") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          344,
          0
        ],
        Error: new Error()
      };
}

if ("variant31" !== "variant31") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          345,
          0
        ],
        Error: new Error()
      };
}

if ("variant32" !== "variant32") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          346,
          0
        ],
        Error: new Error()
      };
}

if ("variant33" !== "variant33") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          347,
          0
        ],
        Error: new Error()
      };
}

if ("variant34" !== "variant34") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          348,
          0
        ],
        Error: new Error()
      };
}

if ("variant35" !== "variant35") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          349,
          0
        ],
        Error: new Error()
      };
}

if ("variant36" !== "variant36") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          350,
          0
        ],
        Error: new Error()
      };
}

if ("variant37" !== "variant37") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          351,
          0
        ],
        Error: new Error()
      };
}

if ("variant38" !== "variant38") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          352,
          0
        ],
        Error: new Error()
      };
}

if ("variant39" !== "variant39") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          353,
          0
        ],
        Error: new Error()
      };
}

if ("variant40" !== "variant40") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          354,
          0
        ],
        Error: new Error()
      };
}

if ("variant41" !== "variant41") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          355,
          0
        ],
        Error: new Error()
      };
}

if ("variant42" !== "variant42") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          356,
          0
        ],
        Error: new Error()
      };
}

if ("variant43" !== "variant43") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          357,
          0
        ],
        Error: new Error()
      };
}

if ("variant44" !== "variant44") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          358,
          0
        ],
        Error: new Error()
      };
}

if ("variant45" !== "variant45") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          359,
          0
        ],
        Error: new Error()
      };
}

if ("variant46" !== "variant46") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          360,
          0
        ],
        Error: new Error()
      };
}

if ("variant47" !== "variant47") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          361,
          0
        ],
        Error: new Error()
      };
}

if ("variant48" !== "variant48") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          362,
          0
        ],
        Error: new Error()
      };
}

if ("variant49" !== "variant49") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          363,
          0
        ],
        Error: new Error()
      };
}

if ("variant50" !== "variant50") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          364,
          0
        ],
        Error: new Error()
      };
}

if ("variant51" !== "variant51") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          365,
          0
        ],
        Error: new Error()
      };
}

if ("variant52" !== "variant52") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          366,
          0
        ],
        Error: new Error()
      };
}

if ("variant53" !== "variant53") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          367,
          0
        ],
        Error: new Error()
      };
}

if ("variant54" !== "variant54") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          368,
          0
        ],
        Error: new Error()
      };
}

if ("variant55" !== "variant55") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          369,
          0
        ],
        Error: new Error()
      };
}

if ("variant56" !== "variant56") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          370,
          0
        ],
        Error: new Error()
      };
}

if ("variant57" !== "variant57") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          371,
          0
        ],
        Error: new Error()
      };
}

if ("variant58" !== "variant58") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          372,
          0
        ],
        Error: new Error()
      };
}

if ("variant59" !== "variant59") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          373,
          0
        ],
        Error: new Error()
      };
}

if ("variant60" !== "variant60") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          374,
          0
        ],
        Error: new Error()
      };
}

if ("variant61" !== "variant61") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          375,
          0
        ],
        Error: new Error()
      };
}

if ("variant62" !== "variant62") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          376,
          0
        ],
        Error: new Error()
      };
}

if ("variant63" !== "variant63") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          377,
          0
        ],
        Error: new Error()
      };
}

if ("variant64" !== "variant64") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          378,
          0
        ],
        Error: new Error()
      };
}

if ("variant65" !== "variant65") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          379,
          0
        ],
        Error: new Error()
      };
}

if ("variant66" !== "variant66") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          380,
          0
        ],
        Error: new Error()
      };
}

if ("variant67" !== "variant67") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          381,
          0
        ],
        Error: new Error()
      };
}

if ("variant68" !== "variant68") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          382,
          0
        ],
        Error: new Error()
      };
}

if ("variant69" !== "variant69") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          383,
          0
        ],
        Error: new Error()
      };
}

if ("variant70" !== "variant70") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          384,
          0
        ],
        Error: new Error()
      };
}

if ("variant71" !== "variant71") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          385,
          0
        ],
        Error: new Error()
      };
}

if ("variant72" !== "variant72") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          386,
          0
        ],
        Error: new Error()
      };
}

if ("variant73" !== "variant73") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          387,
          0
        ],
        Error: new Error()
      };
}

if ("variant74" !== "variant74") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          388,
          0
        ],
        Error: new Error()
      };
}

if ("variant75" !== "variant75") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          389,
          0
        ],
        Error: new Error()
      };
}

if ("variant76" !== "variant76") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          390,
          0
        ],
        Error: new Error()
      };
}

if ("variant77" !== "variant77") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          391,
          0
        ],
        Error: new Error()
      };
}

if ("variant78" !== "variant78") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          392,
          0
        ],
        Error: new Error()
      };
}

if ("variant79" !== "variant79") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          393,
          0
        ],
        Error: new Error()
      };
}

if ("variant80" !== "variant80") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          394,
          0
        ],
        Error: new Error()
      };
}

if ("variant81" !== "variant81") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          395,
          0
        ],
        Error: new Error()
      };
}

if ("variant82" !== "variant82") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          396,
          0
        ],
        Error: new Error()
      };
}

if ("variant83" !== "variant83") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          397,
          0
        ],
        Error: new Error()
      };
}

if ("variant84" !== "variant84") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          398,
          0
        ],
        Error: new Error()
      };
}

if ("variant85" !== "variant85") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          399,
          0
        ],
        Error: new Error()
      };
}

if ("variant86" !== "variant86") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          400,
          0
        ],
        Error: new Error()
      };
}

if ("variant87" !== "variant87") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          401,
          0
        ],
        Error: new Error()
      };
}

if ("variant88" !== "variant88") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          402,
          0
        ],
        Error: new Error()
      };
}

if ("variant89" !== "variant89") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          403,
          0
        ],
        Error: new Error()
      };
}

if ("variant90" !== "variant90") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          404,
          0
        ],
        Error: new Error()
      };
}

if ("variant91" !== "variant91") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          405,
          0
        ],
        Error: new Error()
      };
}

if ("variant92" !== "variant92") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          406,
          0
        ],
        Error: new Error()
      };
}

if ("variant93" !== "variant93") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          407,
          0
        ],
        Error: new Error()
      };
}

if ("variant94" !== "variant94") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          408,
          0
        ],
        Error: new Error()
      };
}

if ("variant95" !== "variant95") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          409,
          0
        ],
        Error: new Error()
      };
}

if ("variant96" !== "variant96") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          410,
          0
        ],
        Error: new Error()
      };
}

if ("variant97" !== "variant97") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          411,
          0
        ],
        Error: new Error()
      };
}

if ("variant98" !== "variant98") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          412,
          0
        ],
        Error: new Error()
      };
}

if ("variant99" !== "variant99") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          413,
          0
        ],
        Error: new Error()
      };
}

if ("variant100" !== "variant100") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          414,
          0
        ],
        Error: new Error()
      };
}

if ("variant101" !== "variant101") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          415,
          0
        ],
        Error: new Error()
      };
}

if ("variant102" !== "variant102") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          416,
          0
        ],
        Error: new Error()
      };
}

if ("variant103" !== "variant103") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          417,
          0
        ],
        Error: new Error()
      };
}

if ("variant104" !== "variant104") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          418,
          0
        ],
        Error: new Error()
      };
}

if ("variant105" !== "variant105") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          419,
          0
        ],
        Error: new Error()
      };
}

if ("variant106" !== "variant106") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          420,
          0
        ],
        Error: new Error()
      };
}

if ("variant107" !== "variant107") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          421,
          0
        ],
        Error: new Error()
      };
}

if ("variant108" !== "variant108") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          422,
          0
        ],
        Error: new Error()
      };
}

if ("variant109" !== "variant109") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          423,
          0
        ],
        Error: new Error()
      };
}

if ("variant110" !== "variant110") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          424,
          0
        ],
        Error: new Error()
      };
}

if ("variant111" !== "variant111") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          425,
          0
        ],
        Error: new Error()
      };
}

if ("variant112" !== "variant112") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          426,
          0
        ],
        Error: new Error()
      };
}

if ("variant113" !== "variant113") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          427,
          0
        ],
        Error: new Error()
      };
}

if ("variant114" !== "variant114") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          428,
          0
        ],
        Error: new Error()
      };
}

if ("variant115" !== "variant115") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          429,
          0
        ],
        Error: new Error()
      };
}

if ("variant116" !== "variant116") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          430,
          0
        ],
        Error: new Error()
      };
}

if ("variant117" !== "variant117") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          431,
          0
        ],
        Error: new Error()
      };
}

if ("variant118" !== "variant118") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          432,
          0
        ],
        Error: new Error()
      };
}

if ("variant119" !== "variant119") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          433,
          0
        ],
        Error: new Error()
      };
}

if ("variant120" !== "variant120") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          434,
          0
        ],
        Error: new Error()
      };
}

if ("variant121" !== "variant121") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          435,
          0
        ],
        Error: new Error()
      };
}

if ("variant122" !== "variant122") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          436,
          0
        ],
        Error: new Error()
      };
}

if ("variant123" !== "variant123") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          437,
          0
        ],
        Error: new Error()
      };
}

if ("variant124" !== "variant124") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          438,
          0
        ],
        Error: new Error()
      };
}

if ("variant125" !== "variant125") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          439,
          0
        ],
        Error: new Error()
      };
}

if ("variant126" !== "variant126") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          440,
          0
        ],
        Error: new Error()
      };
}

if ("variant127" !== "variant127") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          441,
          0
        ],
        Error: new Error()
      };
}

if ("variant128" !== "variant128") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          442,
          0
        ],
        Error: new Error()
      };
}

if ("variant129" !== "variant129") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          443,
          0
        ],
        Error: new Error()
      };
}

if ("variant130" !== "variant130") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          444,
          0
        ],
        Error: new Error()
      };
}

if ("variant131" !== "variant131") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          445,
          0
        ],
        Error: new Error()
      };
}

if ("variant132" !== "variant132") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          446,
          0
        ],
        Error: new Error()
      };
}

if ("variant133" !== "variant133") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          447,
          0
        ],
        Error: new Error()
      };
}

if ("variant134" !== "variant134") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          448,
          0
        ],
        Error: new Error()
      };
}

if ("variant135" !== "variant135") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          449,
          0
        ],
        Error: new Error()
      };
}

if ("variant136" !== "variant136") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          450,
          0
        ],
        Error: new Error()
      };
}

if ("variant137" !== "variant137") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          451,
          0
        ],
        Error: new Error()
      };
}

if ("variant138" !== "variant138") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          452,
          0
        ],
        Error: new Error()
      };
}

if ("variant139" !== "variant139") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          453,
          0
        ],
        Error: new Error()
      };
}

if ("variant140" !== "variant140") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          454,
          0
        ],
        Error: new Error()
      };
}

if ("variant141" !== "variant141") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          455,
          0
        ],
        Error: new Error()
      };
}

if ("variant142" !== "variant142") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          456,
          0
        ],
        Error: new Error()
      };
}

if ("variant143" !== "variant143") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          457,
          0
        ],
        Error: new Error()
      };
}

if ("variant144" !== "variant144") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          458,
          0
        ],
        Error: new Error()
      };
}

if ("variant145" !== "variant145") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          459,
          0
        ],
        Error: new Error()
      };
}

if ("variant146" !== "variant146") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          460,
          0
        ],
        Error: new Error()
      };
}

if ("variant147" !== "variant147") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          461,
          0
        ],
        Error: new Error()
      };
}

if ("variant148" !== "variant148") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          462,
          0
        ],
        Error: new Error()
      };
}

if ("variant149" !== "variant149") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          463,
          0
        ],
        Error: new Error()
      };
}

if ("variant150" !== "variant150") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          464,
          0
        ],
        Error: new Error()
      };
}

if ("variant151" !== "variant151") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          465,
          0
        ],
        Error: new Error()
      };
}

if ("variant152" !== "variant152") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          466,
          0
        ],
        Error: new Error()
      };
}

if ("variant153" !== "variant153") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          467,
          0
        ],
        Error: new Error()
      };
}

if ("variant154" !== "variant154") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          468,
          0
        ],
        Error: new Error()
      };
}

if ("variant155" !== "variant155") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          469,
          0
        ],
        Error: new Error()
      };
}

if ("variant156" !== "variant156") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          470,
          0
        ],
        Error: new Error()
      };
}

if ("variant157" !== "variant157") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          471,
          0
        ],
        Error: new Error()
      };
}

if ("variant158" !== "variant158") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          472,
          0
        ],
        Error: new Error()
      };
}

if ("variant159" !== "variant159") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          473,
          0
        ],
        Error: new Error()
      };
}

if ("variant160" !== "variant160") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          474,
          0
        ],
        Error: new Error()
      };
}

if ("variant161" !== "variant161") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          475,
          0
        ],
        Error: new Error()
      };
}

if ("variant162" !== "variant162") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          476,
          0
        ],
        Error: new Error()
      };
}

if ("variant163" !== "variant163") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          477,
          0
        ],
        Error: new Error()
      };
}

if ("variant164" !== "variant164") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          478,
          0
        ],
        Error: new Error()
      };
}

if ("variant165" !== "variant165") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          479,
          0
        ],
        Error: new Error()
      };
}

if ("variant166" !== "variant166") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          480,
          0
        ],
        Error: new Error()
      };
}

if ("variant167" !== "variant167") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          481,
          0
        ],
        Error: new Error()
      };
}

if ("variant168" !== "variant168") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          482,
          0
        ],
        Error: new Error()
      };
}

if ("variant169" !== "variant169") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          483,
          0
        ],
        Error: new Error()
      };
}

if ("variant170" !== "variant170") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          484,
          0
        ],
        Error: new Error()
      };
}

if ("variant171" !== "variant171") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          485,
          0
        ],
        Error: new Error()
      };
}

if ("variant172" !== "variant172") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          486,
          0
        ],
        Error: new Error()
      };
}

if ("variant173" !== "variant173") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          487,
          0
        ],
        Error: new Error()
      };
}

if ("variant174" !== "variant174") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          488,
          0
        ],
        Error: new Error()
      };
}

if ("variant175" !== "variant175") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          489,
          0
        ],
        Error: new Error()
      };
}

if ("variant176" !== "variant176") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          490,
          0
        ],
        Error: new Error()
      };
}

if ("variant177" !== "variant177") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          491,
          0
        ],
        Error: new Error()
      };
}

if ("variant178" !== "variant178") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          492,
          0
        ],
        Error: new Error()
      };
}

if ("variant179" !== "variant179") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          493,
          0
        ],
        Error: new Error()
      };
}

if ("variant180" !== "variant180") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          494,
          0
        ],
        Error: new Error()
      };
}

if ("variant181" !== "variant181") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          495,
          0
        ],
        Error: new Error()
      };
}

if ("variant182" !== "variant182") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          496,
          0
        ],
        Error: new Error()
      };
}

if ("variant183" !== "variant183") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          497,
          0
        ],
        Error: new Error()
      };
}

if ("variant184" !== "variant184") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          498,
          0
        ],
        Error: new Error()
      };
}

if ("variant185" !== "variant185") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          499,
          0
        ],
        Error: new Error()
      };
}

if ("variant186" !== "variant186") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          500,
          0
        ],
        Error: new Error()
      };
}

if ("variant187" !== "variant187") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          501,
          0
        ],
        Error: new Error()
      };
}

if ("variant188" !== "variant188") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          502,
          0
        ],
        Error: new Error()
      };
}

if ("variant189" !== "variant189") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          503,
          0
        ],
        Error: new Error()
      };
}

if ("variant190" !== "variant190") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          504,
          0
        ],
        Error: new Error()
      };
}

if ("variant191" !== "variant191") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          505,
          0
        ],
        Error: new Error()
      };
}

if ("variant192" !== "variant192") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          506,
          0
        ],
        Error: new Error()
      };
}

if ("variant193" !== "variant193") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          507,
          0
        ],
        Error: new Error()
      };
}

if ("variant194" !== "variant194") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          508,
          0
        ],
        Error: new Error()
      };
}

if ("variant195" !== "variant195") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          509,
          0
        ],
        Error: new Error()
      };
}

if ("variant196" !== "variant196") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          510,
          0
        ],
        Error: new Error()
      };
}

if ("variant197" !== "variant197") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          511,
          0
        ],
        Error: new Error()
      };
}

if ("variant198" !== "variant198") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          512,
          0
        ],
        Error: new Error()
      };
}

if ("variant199" !== "variant199") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          513,
          0
        ],
        Error: new Error()
      };
}

if ("variant200" !== "variant200") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          514,
          0
        ],
        Error: new Error()
      };
}

if ("variant201" !== "variant201") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          515,
          0
        ],
        Error: new Error()
      };
}

if ("variant202" !== "variant202") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          516,
          0
        ],
        Error: new Error()
      };
}

if ("variant203" !== "variant203") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          517,
          0
        ],
        Error: new Error()
      };
}

if ("variant204" !== "variant204") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          518,
          0
        ],
        Error: new Error()
      };
}

if ("variant205" !== "variant205") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          519,
          0
        ],
        Error: new Error()
      };
}

if ("variant206" !== "variant206") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          520,
          0
        ],
        Error: new Error()
      };
}

if ("variant207" !== "variant207") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          521,
          0
        ],
        Error: new Error()
      };
}

if ("variant208" !== "variant208") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          522,
          0
        ],
        Error: new Error()
      };
}

if ("variant209" !== "variant209") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          523,
          0
        ],
        Error: new Error()
      };
}

if ("variant210" !== "variant210") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          524,
          0
        ],
        Error: new Error()
      };
}

if ("variant211" !== "variant211") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          525,
          0
        ],
        Error: new Error()
      };
}

if ("variant212" !== "variant212") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          526,
          0
        ],
        Error: new Error()
      };
}

if ("variant213" !== "variant213") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          527,
          0
        ],
        Error: new Error()
      };
}

if ("variant214" !== "variant214") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          528,
          0
        ],
        Error: new Error()
      };
}

if ("variant215" !== "variant215") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          529,
          0
        ],
        Error: new Error()
      };
}

if ("variant216" !== "variant216") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          530,
          0
        ],
        Error: new Error()
      };
}

if ("variant217" !== "variant217") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          531,
          0
        ],
        Error: new Error()
      };
}

if ("variant218" !== "variant218") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          532,
          0
        ],
        Error: new Error()
      };
}

if ("variant219" !== "variant219") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          533,
          0
        ],
        Error: new Error()
      };
}

if ("variant220" !== "variant220") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          534,
          0
        ],
        Error: new Error()
      };
}

if ("variant221" !== "variant221") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          535,
          0
        ],
        Error: new Error()
      };
}

if ("variant222" !== "variant222") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          536,
          0
        ],
        Error: new Error()
      };
}

if ("variant223" !== "variant223") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          537,
          0
        ],
        Error: new Error()
      };
}

if ("variant224" !== "variant224") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          538,
          0
        ],
        Error: new Error()
      };
}

if ("variant225" !== "variant225") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          539,
          0
        ],
        Error: new Error()
      };
}

if ("variant226" !== "variant226") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          540,
          0
        ],
        Error: new Error()
      };
}

if ("variant227" !== "variant227") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          541,
          0
        ],
        Error: new Error()
      };
}

if ("variant228" !== "variant228") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          542,
          0
        ],
        Error: new Error()
      };
}

if ("variant229" !== "variant229") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          543,
          0
        ],
        Error: new Error()
      };
}

if ("variant230" !== "variant230") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          544,
          0
        ],
        Error: new Error()
      };
}

if ("variant231" !== "variant231") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          545,
          0
        ],
        Error: new Error()
      };
}

if ("variant232" !== "variant232") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          546,
          0
        ],
        Error: new Error()
      };
}

if ("variant233" !== "variant233") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          547,
          0
        ],
        Error: new Error()
      };
}

if ("variant234" !== "variant234") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          548,
          0
        ],
        Error: new Error()
      };
}

if ("variant235" !== "variant235") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          549,
          0
        ],
        Error: new Error()
      };
}

if ("variant236" !== "variant236") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          550,
          0
        ],
        Error: new Error()
      };
}

if ("variant237" !== "variant237") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          551,
          0
        ],
        Error: new Error()
      };
}

if ("variant238" !== "variant238") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          552,
          0
        ],
        Error: new Error()
      };
}

if ("variant239" !== "variant239") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          553,
          0
        ],
        Error: new Error()
      };
}

if ("variant240" !== "variant240") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          554,
          0
        ],
        Error: new Error()
      };
}

if ("variant241" !== "variant241") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          555,
          0
        ],
        Error: new Error()
      };
}

if ("variant242" !== "variant242") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          556,
          0
        ],
        Error: new Error()
      };
}

if ("variant243" !== "variant243") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          557,
          0
        ],
        Error: new Error()
      };
}

if ("variant244" !== "variant244") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          558,
          0
        ],
        Error: new Error()
      };
}

if ("variant245" !== "variant245") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          559,
          0
        ],
        Error: new Error()
      };
}

if ("variant246" !== "variant246") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          560,
          0
        ],
        Error: new Error()
      };
}

if ("variant247" !== "variant247") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          561,
          0
        ],
        Error: new Error()
      };
}

if ("variant248" !== "variant248") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          562,
          0
        ],
        Error: new Error()
      };
}

if ("variant249" !== "variant249") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          563,
          0
        ],
        Error: new Error()
      };
}

if ("variant250" !== "variant250") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          564,
          0
        ],
        Error: new Error()
      };
}

if ("variant251" !== "variant251") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          565,
          0
        ],
        Error: new Error()
      };
}

if ("variant252" !== "variant252") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          566,
          0
        ],
        Error: new Error()
      };
}

if ("variant253" !== "variant253") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          567,
          0
        ],
        Error: new Error()
      };
}

if ("variant254" !== "variant254") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          568,
          0
        ],
        Error: new Error()
      };
}

if ("variant255" !== "variant255") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          569,
          0
        ],
        Error: new Error()
      };
}

if ("variant256" !== "variant256") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          570,
          0
        ],
        Error: new Error()
      };
}

if ("variant257" !== "variant257") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          571,
          0
        ],
        Error: new Error()
      };
}

if ("variant258" !== "variant258") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          572,
          0
        ],
        Error: new Error()
      };
}

if ("variant259" !== "variant259") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          573,
          0
        ],
        Error: new Error()
      };
}

if ("variant260" !== "variant260") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          574,
          0
        ],
        Error: new Error()
      };
}

if ("variant261" !== "variant261") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          575,
          0
        ],
        Error: new Error()
      };
}

if ("variant262" !== "variant262") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          576,
          0
        ],
        Error: new Error()
      };
}

if ("variant263" !== "variant263") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          577,
          0
        ],
        Error: new Error()
      };
}

if ("variant264" !== "variant264") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          578,
          0
        ],
        Error: new Error()
      };
}

if ("variant265" !== "variant265") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          579,
          0
        ],
        Error: new Error()
      };
}

if ("variant266" !== "variant266") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          580,
          0
        ],
        Error: new Error()
      };
}

if ("variant267" !== "variant267") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          581,
          0
        ],
        Error: new Error()
      };
}

if ("variant268" !== "variant268") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          582,
          0
        ],
        Error: new Error()
      };
}

if ("variant269" !== "variant269") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          583,
          0
        ],
        Error: new Error()
      };
}

if ("variant270" !== "variant270") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          584,
          0
        ],
        Error: new Error()
      };
}

if ("variant271" !== "variant271") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          585,
          0
        ],
        Error: new Error()
      };
}

if ("variant272" !== "variant272") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          586,
          0
        ],
        Error: new Error()
      };
}

if ("variant273" !== "variant273") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          587,
          0
        ],
        Error: new Error()
      };
}

if ("variant274" !== "variant274") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          588,
          0
        ],
        Error: new Error()
      };
}

if ("variant275" !== "variant275") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          589,
          0
        ],
        Error: new Error()
      };
}

if ("variant276" !== "variant276") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          590,
          0
        ],
        Error: new Error()
      };
}

if ("variant277" !== "variant277") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          591,
          0
        ],
        Error: new Error()
      };
}

if ("variant278" !== "variant278") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          592,
          0
        ],
        Error: new Error()
      };
}

if ("variant279" !== "variant279") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          593,
          0
        ],
        Error: new Error()
      };
}

if ("variant280" !== "variant280") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          594,
          0
        ],
        Error: new Error()
      };
}

if ("variant281" !== "variant281") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          595,
          0
        ],
        Error: new Error()
      };
}

if ("variant282" !== "variant282") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          596,
          0
        ],
        Error: new Error()
      };
}

if ("variant283" !== "variant283") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          597,
          0
        ],
        Error: new Error()
      };
}

if ("variant284" !== "variant284") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          598,
          0
        ],
        Error: new Error()
      };
}

if ("variant285" !== "variant285") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          599,
          0
        ],
        Error: new Error()
      };
}

if ("variant286" !== "variant286") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          600,
          0
        ],
        Error: new Error()
      };
}

if ("variant287" !== "variant287") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          601,
          0
        ],
        Error: new Error()
      };
}

if ("variant288" !== "variant288") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          602,
          0
        ],
        Error: new Error()
      };
}

if ("variant289" !== "variant289") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          603,
          0
        ],
        Error: new Error()
      };
}

if ("variant290" !== "variant290") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          604,
          0
        ],
        Error: new Error()
      };
}

if ("variant291" !== "variant291") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          605,
          0
        ],
        Error: new Error()
      };
}

if ("variant292" !== "variant292") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          606,
          0
        ],
        Error: new Error()
      };
}

if ("variant293" !== "variant293") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          607,
          0
        ],
        Error: new Error()
      };
}

if ("variant294" !== "variant294") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          608,
          0
        ],
        Error: new Error()
      };
}

if ("variant295" !== "variant295") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          609,
          0
        ],
        Error: new Error()
      };
}

if ("variant296" !== "variant296") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          610,
          0
        ],
        Error: new Error()
      };
}

if ("variant297" !== "variant297") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          611,
          0
        ],
        Error: new Error()
      };
}

if ("variant298" !== "variant298") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          612,
          0
        ],
        Error: new Error()
      };
}

if ("variant299" !== "variant299") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          613,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant0"), "variant0")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          614,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant1"), "variant1")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          615,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant2"), "variant2")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          616,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant3"), "variant3")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          617,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant4"), "variant4")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          618,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant5"), "variant5")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          619,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant6"), "variant6")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          620,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant7"), "variant7")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          621,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant8"), "variant8")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          622,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant9"), "variant9")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          623,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant10"), "variant10")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          624,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant11"), "variant11")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          625,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant12"), "variant12")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          626,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant13"), "variant13")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          627,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant14"), "variant14")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          628,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant15"), "variant15")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          629,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant16"), "variant16")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          630,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant17"), "variant17")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          631,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant18"), "variant18")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          632,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant19"), "variant19")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          633,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant20"), "variant20")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          634,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant21"), "variant21")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          635,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant22"), "variant22")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          636,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant23"), "variant23")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          637,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant24"), "variant24")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          638,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant25"), "variant25")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          639,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant26"), "variant26")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          640,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant27"), "variant27")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          641,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant28"), "variant28")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          642,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant29"), "variant29")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          643,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant30"), "variant30")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          644,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant31"), "variant31")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          645,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant32"), "variant32")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          646,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant33"), "variant33")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          647,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant34"), "variant34")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          648,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant35"), "variant35")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          649,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant36"), "variant36")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          650,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant37"), "variant37")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          651,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant38"), "variant38")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          652,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant39"), "variant39")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          653,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant40"), "variant40")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          654,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant41"), "variant41")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          655,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant42"), "variant42")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          656,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant43"), "variant43")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          657,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant44"), "variant44")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          658,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant45"), "variant45")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          659,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant46"), "variant46")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          660,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant47"), "variant47")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          661,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant48"), "variant48")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          662,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant49"), "variant49")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          663,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant50"), "variant50")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          664,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant51"), "variant51")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          665,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant52"), "variant52")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          666,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant53"), "variant53")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          667,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant54"), "variant54")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          668,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant55"), "variant55")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          669,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant56"), "variant56")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          670,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant57"), "variant57")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          671,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant58"), "variant58")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          672,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant59"), "variant59")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          673,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant60"), "variant60")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          674,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant61"), "variant61")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          675,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant62"), "variant62")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          676,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant63"), "variant63")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          677,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant64"), "variant64")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          678,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant65"), "variant65")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          679,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant66"), "variant66")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          680,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant67"), "variant67")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          681,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant68"), "variant68")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          682,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant69"), "variant69")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          683,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant70"), "variant70")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          684,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant71"), "variant71")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          685,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant72"), "variant72")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          686,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant73"), "variant73")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          687,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant74"), "variant74")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          688,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant75"), "variant75")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          689,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant76"), "variant76")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          690,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant77"), "variant77")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          691,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant78"), "variant78")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          692,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant79"), "variant79")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          693,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant80"), "variant80")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          694,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant81"), "variant81")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          695,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant82"), "variant82")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          696,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant83"), "variant83")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          697,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant84"), "variant84")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          698,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant85"), "variant85")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          699,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant86"), "variant86")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          700,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant87"), "variant87")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          701,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant88"), "variant88")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          702,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant89"), "variant89")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          703,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant90"), "variant90")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          704,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant91"), "variant91")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          705,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant92"), "variant92")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          706,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant93"), "variant93")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          707,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant94"), "variant94")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          708,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant95"), "variant95")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          709,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant96"), "variant96")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          710,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant97"), "variant97")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          711,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant98"), "variant98")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          712,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant99"), "variant99")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          713,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant100"), "variant100")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          714,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant101"), "variant101")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          715,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant102"), "variant102")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          716,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant103"), "variant103")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          717,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant104"), "variant104")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          718,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant105"), "variant105")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          719,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant106"), "variant106")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          720,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant107"), "variant107")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          721,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant108"), "variant108")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          722,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant109"), "variant109")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          723,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant110"), "variant110")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          724,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant111"), "variant111")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          725,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant112"), "variant112")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          726,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant113"), "variant113")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          727,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant114"), "variant114")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          728,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant115"), "variant115")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          729,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant116"), "variant116")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          730,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant117"), "variant117")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          731,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant118"), "variant118")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          732,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant119"), "variant119")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          733,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant120"), "variant120")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          734,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant121"), "variant121")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          735,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant122"), "variant122")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          736,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant123"), "variant123")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          737,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant124"), "variant124")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          738,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant125"), "variant125")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          739,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant126"), "variant126")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          740,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant127"), "variant127")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          741,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant128"), "variant128")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          742,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant129"), "variant129")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          743,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant130"), "variant130")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          744,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant131"), "variant131")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          745,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant132"), "variant132")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          746,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant133"), "variant133")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          747,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant134"), "variant134")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          748,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant135"), "variant135")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          749,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant136"), "variant136")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          750,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant137"), "variant137")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          751,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant138"), "variant138")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          752,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant139"), "variant139")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          753,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant140"), "variant140")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          754,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant141"), "variant141")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          755,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant142"), "variant142")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          756,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant143"), "variant143")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          757,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant144"), "variant144")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          758,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant145"), "variant145")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          759,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant146"), "variant146")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          760,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant147"), "variant147")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          761,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant148"), "variant148")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          762,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant149"), "variant149")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          763,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant150"), "variant150")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          764,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant151"), "variant151")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          765,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant152"), "variant152")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          766,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant153"), "variant153")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          767,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant154"), "variant154")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          768,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant155"), "variant155")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          769,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant156"), "variant156")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          770,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant157"), "variant157")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          771,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant158"), "variant158")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          772,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant159"), "variant159")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          773,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant160"), "variant160")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          774,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant161"), "variant161")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          775,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant162"), "variant162")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          776,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant163"), "variant163")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          777,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant164"), "variant164")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          778,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant165"), "variant165")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          779,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant166"), "variant166")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          780,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant167"), "variant167")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          781,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant168"), "variant168")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          782,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant169"), "variant169")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          783,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant170"), "variant170")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          784,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant171"), "variant171")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          785,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant172"), "variant172")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          786,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant173"), "variant173")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          787,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant174"), "variant174")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          788,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant175"), "variant175")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          789,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant176"), "variant176")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          790,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant177"), "variant177")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          791,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant178"), "variant178")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          792,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant179"), "variant179")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          793,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant180"), "variant180")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          794,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant181"), "variant181")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          795,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant182"), "variant182")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          796,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant183"), "variant183")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          797,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant184"), "variant184")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          798,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant185"), "variant185")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          799,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant186"), "variant186")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          800,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant187"), "variant187")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          801,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant188"), "variant188")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          802,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant189"), "variant189")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          803,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant190"), "variant190")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          804,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant191"), "variant191")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          805,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant192"), "variant192")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          806,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant193"), "variant193")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          807,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant194"), "variant194")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          808,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant195"), "variant195")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          809,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant196"), "variant196")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          810,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant197"), "variant197")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          811,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant198"), "variant198")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          812,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant199"), "variant199")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          813,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant200"), "variant200")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          814,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant201"), "variant201")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          815,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant202"), "variant202")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          816,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant203"), "variant203")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          817,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant204"), "variant204")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          818,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant205"), "variant205")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          819,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant206"), "variant206")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          820,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant207"), "variant207")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          821,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant208"), "variant208")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          822,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant209"), "variant209")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          823,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant210"), "variant210")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          824,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant211"), "variant211")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          825,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant212"), "variant212")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          826,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant213"), "variant213")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          827,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant214"), "variant214")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          828,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant215"), "variant215")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          829,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant216"), "variant216")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          830,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant217"), "variant217")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          831,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant218"), "variant218")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          832,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant219"), "variant219")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          833,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant220"), "variant220")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          834,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant221"), "variant221")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          835,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant222"), "variant222")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          836,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant223"), "variant223")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          837,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant224"), "variant224")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          838,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant225"), "variant225")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          839,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant226"), "variant226")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          840,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant227"), "variant227")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          841,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant228"), "variant228")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          842,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant229"), "variant229")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          843,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant230"), "variant230")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          844,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant231"), "variant231")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          845,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant232"), "variant232")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          846,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant233"), "variant233")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          847,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant234"), "variant234")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          848,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant235"), "variant235")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          849,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant236"), "variant236")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          850,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant237"), "variant237")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          851,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant238"), "variant238")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          852,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant239"), "variant239")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          853,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant240"), "variant240")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          854,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant241"), "variant241")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          855,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant242"), "variant242")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          856,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant243"), "variant243")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          857,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant244"), "variant244")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          858,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant245"), "variant245")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          859,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant246"), "variant246")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          860,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant247"), "variant247")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          861,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant248"), "variant248")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          862,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant249"), "variant249")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          863,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant250"), "variant250")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          864,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant251"), "variant251")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          865,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant252"), "variant252")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          866,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant253"), "variant253")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          867,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant254"), "variant254")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          868,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant255"), "variant255")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          869,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant256"), "variant256")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          870,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant257"), "variant257")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          871,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant258"), "variant258")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          872,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant259"), "variant259")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          873,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant260"), "variant260")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          874,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant261"), "variant261")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          875,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant262"), "variant262")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          876,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant263"), "variant263")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          877,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant264"), "variant264")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          878,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant265"), "variant265")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          879,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant266"), "variant266")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          880,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant267"), "variant267")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          881,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant268"), "variant268")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          882,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant269"), "variant269")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          883,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant270"), "variant270")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          884,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant271"), "variant271")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          885,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant272"), "variant272")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          886,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant273"), "variant273")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          887,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant274"), "variant274")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          888,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant275"), "variant275")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          889,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant276"), "variant276")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          890,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant277"), "variant277")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          891,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant278"), "variant278")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          892,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant279"), "variant279")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          893,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant280"), "variant280")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          894,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant281"), "variant281")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          895,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant282"), "variant282")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          896,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant283"), "variant283")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          897,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant284"), "variant284")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          898,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant285"), "variant285")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          899,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant286"), "variant286")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          900,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant287"), "variant287")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          901,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant288"), "variant288")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          902,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant289"), "variant289")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          903,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant290"), "variant290")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          904,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant291"), "variant291")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          905,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant292"), "variant292")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          906,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant293"), "variant293")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          907,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant294"), "variant294")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          908,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant295"), "variant295")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          909,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant296"), "variant296")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          910,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant297"), "variant297")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          911,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant298"), "variant298")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          912,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant299"), "variant299")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          913,
          0
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("xx"), undefined)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.res",
          914,
          0
        ],
        Error: new Error()
      };
}

exports.tToJs = tToJs;
exports.tFromJs = tFromJs;
exports.eq = eq;
/*  Not a pure module */
