type challenge = {
  id: int,
  name: string,
  description: string,
};

type testnet = {
  name: string,
  is_active: bool,
};

external parseChallenge: Js.Json.t => challenge = "%identity";
external parseTestnet: Js.Json.t => testnet = "%identity";

let fetchArray = endpoint => {
  ReFetch.fetch(
    "http://points.o1test.net/api/v1/" ++ endpoint,
    ~method_=Get,
    ~headers={
      "Accept": "application/json",
      "Content-Type": "application/json",
    },
  )
  |> Promise.bind(Bs_fetch.Response.json)
  |> Promise.map(r => {
       let results =
         Option.bind(Js.Json.decodeObject(r), o =>
           Js.Dict.get(o, "results")
         );
       switch (Option.bind(results, Js.Json.decodeArray)) {
       | Some(arr) => arr
       | None => [||]
       };
     });
};

let fetchTestnet = uri =>
  fetchArray(uri)
  |> Promise.map(Array.map(parseTestnet))
  |> Promise.map(a => {
       Array.to_list(a)
       |> List.find_opt(t => t.is_active)
       |> Option.map(t => t.name)
     });

let fetchChallenges = uri =>
  Promise.map(Array.map(parseChallenge), fetchArray(uri));

// Used in getInitialProps of Testnet.re
let fetchAllChallenges = () => {
  Js.Promise.all4((
    fetchTestnet("testnets/"),
    fetchChallenges("ranking-challenges/"),
    fetchChallenges("continuous-challenges/"),
    fetchChallenges("threshold-challenges/"),
  ));
};

module Styles = {
  open Css;
  let weekHeader =
    merge([Theme.H2.basic, style([padding2(~v=`rem(1.), ~h=`zero)])]);
  let header =
    merge([
      Theme.H4.wide,
      style([textAlign(`left), fontSize(`rem(1.)), fontWeight(`light)]),
    ]);
};

let renderChallenges = (challenges: array(challenge)) => {
  Array.map(
    (c: challenge) =>
      <div key={string_of_int(c.id)}>
        <h4> {React.string(c.name)} </h4>
        <p> {React.string(c.description)} </p>
      </div>,
    challenges,
  )
  |> React.array;
};

[@react.component]
let make = (~challenges as (testnetName, ranking, continuous, threshold)) => {
  switch (testnetName) {
  | None =>
    <h2 className=Styles.weekHeader> {React.string("No active testnet")} </h2>
  | Some(testnet) =>
    <>
      <h2 className=Styles.weekHeader> {React.string(testnet)} </h2>
      {renderChallenges(ranking)}
      {renderChallenges(continuous)}
      {renderChallenges(threshold)}
    </>
  };
};
