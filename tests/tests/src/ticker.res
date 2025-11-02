open Belt

/** General purpose utility functions
  */
module Util = {
  let split = (~delim, s) => {
    let rec loop = (l, x) =>
      switch x {
      | 0 => l
      | i =>
        switch Js.String2.lastIndexOfFrom(s, delim, i - 1) {
        | -1 => list{Js.String2.substrAtMost(s, ~from=0, ~length=i), ...l}
        | i' =>
          let l = list{Js.String2.substrAtMost(s, ~from=i' + 1, ~length=i - i' - 1), ...l}
          let l = if i' == 0 {
            list{"", ...l}
          } else {
            l
          }
          loop(l, i')
        }
      }

    let len = Js.String2.length(s)
    switch len {
    | 0 => list{}
    | _ => loop(list{}, len)
    }
  }

  let string_of_float_option = x =>
    switch x {
    | Some(x) => Js.Float.toString(x)
    | None => "nan"
    }
}

/* AST for the tickers */

type binary_op =
  | PLUS
  | MINUS

type rank =
  | Uninitialized
  | Visited
  | Ranked(int)

type rec binary_op_ticker = {
  op: binary_op,
  rhs: ticker,
  lhs: ticker,
}

and ticker_type =
  | Market
  | Binary_op(binary_op_ticker)

and ticker = {
  mutable value: option<float>,
  mutable rank: rank,
  ticker_name: string,
  type_: ticker_type,
}

let string_of_rank = x =>
  switch x {
  | Uninitialized => "Uninitialized"
  | Visited => "Visited"
  | Ranked(i) => "Ranked(" ++ __unsafe_cast(i) ++ ")"
  }

let find_ticker_by_name = (all_tickers, ticker) =>
  all_tickers->List.getBy(({ticker_name, _}) => ticker_name == ticker)->Option.getExn

let print_all_composite = all_tickers =>
  all_tickers->List.forEach(x =>
    switch x {
    | {type_: Market, _} => ()
    | {type_: Binary_op(_), ticker_name, value} =>
      switch value {
      | Some(v) => Console.log(ticker_name)
      | None => Console.log(ticker_name)
      }
    }
  )

module Ticker_map = Map.String

/** For each market tickers, this function will compute 
    the associated list of tickers value to be updated
    based on the correct graph ordering 

    We first rank all the tickers with a depth first search 
    algorithm (lowest rank for the deepest nodes). 

    We then collect all the tickers which depends on each of the 
    market tickers and finally we `sort_uniq` that list by rank to 
    guarantee that a composite ticker is update only once and in 
    the correct order. 
 */
let compute_update_sequences = all_tickers => {
  /* Ranking */

  all_tickers
  ->List.reduceReverse(0, (counter, ticker) => {
    let rec loop = (counter, {rank, _} as ticker) =>
      switch rank {
      | Ranked(_) => counter
      | Visited => counter
      | Uninitialized =>
        ticker.rank = Visited
        switch ticker.type_ {
        | Market =>
          let counter = counter + 1
          ticker.rank = Ranked(counter)
          counter
        | Binary_op({lhs, rhs, _}) =>
          let counter = loop(counter, lhs)
          let counter = loop(counter, rhs)
          let counter = counter + 1
          ticker.rank = Ranked(counter)
          counter
        }
      }

    loop(counter, ticker)
  })
  ->ignore

  /* collect all dependencies of market tickers */

  let map =
    all_tickers
    ->List.reverse
    ->List.reduceReverse(Ticker_map.empty, (map, {ticker_name, type_, _} as ticker) =>
      switch type_ {
      | Market => map->Ticker_map.set(ticker_name, list{ticker})
      | _ =>
        let rec loop = (up, map, {ticker_name, type_} as ticker) =>
          switch type_ {
          | Market =>
            let l = map->Ticker_map.getExn(ticker_name)
            map->Ticker_map.set(ticker_name, \"@"(up, l))
          | Binary_op({lhs, rhs, _}) =>
            let map = loop(list{ticker, ...up}, map, lhs)
            loop(list{ticker, ...up}, map, rhs)
          }

        loop(list{}, map, ticker)
      }
    )
  /* `List.rev is needed to process the node in the order they were processed
      TODO: this code should be more robust
 */

  /* order dependencies based on rank */

  map->Ticker_map.reduce(map, (map, k, l) => {
    let l = l->List.sort((lhs, rhs) =>
      switch (lhs, rhs) {
      | ({rank: Ranked(x)}, {rank: Ranked(y)}) => Pervasives.compare(x, y)
      | (_, _) => failwith("All nodes should be ranked")
      }
    )
    map->Ticker_map.set(k, l)
  })
}

/** Process a new quote for a market ticker 
 */
let process_quote = (ticker_map, new_ticker, new_value) => {
  let update_sequence = ticker_map->Ticker_map.getExn(new_ticker)

  update_sequence->List.forEach(ticker =>
    switch ticker {
    | {type_: Market, ticker_name, _} if ticker_name == new_ticker => ticker.value = Some(new_value)

    | {type_: Market, _} => failwith("Only single Market ticker should be udpated upon a new quote")

    | {type_: Binary_op({lhs, rhs, op}), ticker_name, _} =>
      let value = switch (lhs.value, rhs.value) {
      | (None, None)
      | (None, _)
      | (_, None) =>
        None
      | (Some(x), Some(y)) =>
        switch op {
        | PLUS => Some(x +. y)
        | MINUS => Some(x -. y)
        }
      }
      ticker.value = value
    }
  )
}

let process_input_line = (ticker_map, all_tickers, line) => {
  let make_binary_op = (ticker_name, lhs, rhs, op) => {
    let lhs = find_ticker_by_name(all_tickers, lhs)
    let rhs = find_ticker_by_name(all_tickers, rhs)
    {
      rank: Uninitialized,
      ticker_name,
      type_: Binary_op({lhs, rhs, op}),
      value: None,
    }
  }

  let tokens = Util.split(~delim="|", line)

  let all_tickers = switch tokens {
  | list{"R", ticker_name, "S"} => (
      list{{ticker_name, rank: Uninitialized, type_: Market, value: None}, ...all_tickers},
      ticker_map,
    )
  | list{"R", ticker_name, "+", lhs, rhs} => (
      list{make_binary_op(ticker_name, lhs, rhs, PLUS), ...all_tickers},
      ticker_map,
    )
  | list{"R", ticker_name, "-", lhs, rhs} => (
      list{make_binary_op(ticker_name, lhs, rhs, MINUS), ...all_tickers},
      ticker_map,
    )
  | list{"Q", ticker_name, value} =>
    let ticker_map = switch ticker_map {
    | Some(ticker_map) => ticker_map
    | None => compute_update_sequences(all_tickers)
    }
    let value = value->Float.fromString->Option.getExn
    process_quote(ticker_map, ticker_name, value)
    (all_tickers, Some(ticker_map))
  | _ => failwith("Invalid input line")
  }

  all_tickers
}

let lines = list{
  "R|MSFT|S",
  "R|IBM|S",
  "R|FB|S",
  "R|CP1|+|MSFT|IBM",
  "R|CP2|-|FB|IBM",
  "R|CP12|+|CP1|CP2",
  "Q|MSFT|120.",
  "Q|IBM|130.",
  "Q|FB|80.",
}

let rec loop = (lines, (all_tickers, ticker_map)) =>
  switch lines {
  | list{line, ...lines} => loop(lines, process_input_line(ticker_map, all_tickers, line))
  | list{} => print_all_composite(all_tickers)
  }
