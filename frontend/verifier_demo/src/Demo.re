module Block = [%graphql
  {|
    query {
      blockchainVerificationKey
      blocks(last: 1) {
        nodes {
          stateHashField
          protocolStateProof {
            a
            b
            c
            delta_prime
            z
          }
        }
      }
    }
  |}
];

module BlockQuery = ReasonApollo.CreateQuery(Block);

module DemoInternal = {
  [@react.component]
  let make = (~worker, ~blocks) => {
    let (isVerified, setVerified) = React.useState(() => None);

    React.useEffect1(
      () =>
        switch (blocks) {
        | None => None
        | Some(data) when Array.length(data##blocks##nodes) == 0 => None
        | Some(data) =>
          Js.log("posting message");
          let block = data##blocks##nodes[0];
          let proof = block##protocolStateProof;
          let msg = {
            "key": data##blockchainVerificationKey,
            "a": proof##a,
            "b": proof##b,
            "c": proof##c,
            "delta_prime": proof##delta_prime,
            "z": proof##z,
            "stateHashField": block##stateHashField,
          };
          Js.log(msg);

          let _ =
            Worker.Promise.postMessage(worker, msg)
            |> Js.Promise.then_(response => {
                 let verified = response##verified;
                 let verifyTime = response##time;
                 Js.log2("got response", response);
                 setVerified(_ => Some((verified, verifyTime)));
                 Js.Promise.resolve();
               });
          setVerified(_ => None);
          None;
        },
      [|blocks|],
    );

    let (verified, verifiedTime) =
      switch (isVerified) {
      | Some((v, time)) => (v, string_of_int(time))
      | None => (false, "?")
      };

    <>
      <Background />
      <Banner time=verifiedTime />
      <Spacer height=5.0 />
      <BlockRow verified />
    </>;
  };
};

[@react.component]
let make = (~worker) => {
  <ReasonApollo.Provider client=Apollo.client>
    <BlockQuery>
      {response =>
         switch (response.result) {
         | Loading => <DemoInternal worker blocks=None />
         | Error(e) =>
           Js.log(e##message);
           <DemoInternal worker blocks=None />;
         | Data(d) => <DemoInternal worker blocks={Some(d)} />
         }}
    </BlockQuery>
  </ReasonApollo.Provider>;
};