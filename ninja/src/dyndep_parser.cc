// Copyright 2015 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "dyndep_parser.h"

#include <vector>

#include "dyndep.h"
#include "graph.h"
#include "state.h"
#include "util.h"
#include "version.h"

DyndepParser::DyndepParser(State* state, DiskInterface* file_reader,
                           DyndepFile* dyndep_file)
    : Parser(state, file_reader)
    , dyndep_file_(dyndep_file) {
}

bool DyndepParser::Parse(const string& filename, const string& input,
                         string* err) {
  lexer_.Start(filename, input);
  for (;;) {
    if(lexer_.EndAfterEatWhiteSpace()){
      return true;
    }
    if(!ParseEdge(err)){
      return false;
    }
  }
  return false;  // not reached
}

bool DyndepParser::ParseEdge(string* err) {
  // Parse one explicit output.  We expect it to already have an edge.
  // We will record its dynamically-discovered dependency information.
  Dyndeps* dyndeps = NULL;
  Edge* edge = NULL;
  {
    string path;
    if(!lexer_.ReadSimplePath(&path)){
      return lexer_.Error("expected path",err);      
    }   
    string path_err;
    uint64_t slash_bits;
    if (!CanonicalizePath(&path, &slash_bits, &path_err))
      return lexer_.Error(path_err, err);
    Node* node = state_->LookupNode(path);
    if (!node || !node->in_edge())
      return lexer_.Error("no build statement exists for '" + path + "'", err);
    edge = node->in_edge();
    std::pair<DyndepFile::iterator, bool> res =
      dyndep_file_->insert(DyndepFile::value_type(edge, Dyndeps()));
    if (!res.second)
      return lexer_.Error("multiple statements for '" + path + "'", err);
    dyndeps = &res.first->second;
  }
  if (!ExpectToken(Lexer::COLON, err))
    return false;

  // Parse implicit inputs, if any.
  vector<string>ins;
  for (;;) {
    string in;
    if (!lexer_.ReadSimplePath(&in)) {
      break;
    }
    ins.push_back(in);
  }
  if (!ExpectToken(Lexer::NEWLINE, err))
    return false;
  dyndeps->implicit_inputs_.reserve(ins.size());
  string path_err;
  for (vector<string>::iterator i = ins.begin(); i != ins.end(); ++i) {
    string path = *i;
    uint64_t slash_bits;
    if (!CanonicalizePath(&path, &slash_bits, &path_err))
      return lexer_.Error(path_err, err);
    Node* n = state_->GetNode(path, slash_bits);
    dyndeps->implicit_inputs_.push_back(n);
  }

  return true;
}
