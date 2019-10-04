open Tc;

let defaultName = "My Wallet";

module AddWallet = [%graphql
  {|
     mutation addWallet($password: String) {
         addWallet(input: {password: $password}) {
           publicKey @bsDecoder(fn: "Apollo.Decoders.publicKey")
         }
     }
   |}
];

module AddWalletMutation = ReasonApollo.CreateMutation(AddWallet);

module Styles = {
  open Css;

  let hero = {
    style([display(`flex), flexDirection(`row)]);
  };

  let heroLeft = {
    style([
      display(`flex),
      flexDirection(`column),
      justifyContent(`center),
      width(`percent(100.0)),
      maxWidth(`rem(28.0)),
      marginLeft(`px(80)),
    ]);
  };

  let heroBody = {
    style([
      marginTop(`rem(2.)),
      marginBottom(`rem(3.)),
      maxWidth(`rem(21.5)),
    ]);
  };
  let buttonRow = {
    style([display(`flex), flexDirection(`row)]);
  };
};

[@react.component]
let make = (~nextStep, ~prevStep) => {
  let (walletName, setName) = React.useState(() => defaultName);
  let (password, setPassword) = React.useState(() => "");

  let (_settings, updateAddressBook) =
    React.useContext(AddressBookProvider.context);
  <div className=Theme.Onboarding.main>
    <div className=Styles.hero>
      <div className=Styles.heroLeft>
        <h1> {React.string("Create Your Account")} </h1>
        <p className=Styles.heroBody>
          {React.string(
             "Create your first account to complete setting up Coda Wallet. Please be sure to choose a secure password.",
           )}
        </p>
        <Spacer height=1. />
        <TextField
          label="Name"
          onChange={value => setName(_ => value)}
          value=walletName
        />
        <Spacer height=0.5 />
        <TextField
          label="Pass"
          type_="password"
          onChange={value => setPassword(_ => value)}
          value=password
        />
        <div className=Css.(style([display(`flex)]))>
          <AddWalletMutation>
            {(mutation, _) =>
               <Button
                 label="Create"
                 style=Button.Green
                 onClick={_ => {
                   let variables = AddWallet.make(~password, ())##variables;
                   let performMutation =
                     Task.liftPromise(() =>
                       mutation(
                         ~variables,
                         ~refetchQueries=[|"getWallets"|],
                         (),
                       )
                     );
                   Task.perform(
                     performMutation,
                     ~f=
                       fun
                       | EmptyResponse => ()
                       | Errors(_) => print_endline("Error adding wallet")
                       | Data(data) => {
                           let key = data##addWallet##publicKey;
                           updateAddressBook(
                             AddressBook.set(~key, ~name=walletName),
                           );
                         },
                   );
                 }}
               />}
          </AddWalletMutation>
        </div>
        <div className=Styles.buttonRow>
          <Button label="Go Back" onClick={_ => prevStep()} />
          <Spacer width=0.5 />
          <Button label="Continue" onClick={_ => nextStep()} />
        </div>
      </div>
      <div
        // Graphic goes here
      />
    </div>
  </div>;
};
