module ApiConfig = {
  type params = {q: string};
  let encode = params => {
    Json.Encode.(object_([("q", string(params.q))]));
  };

  type response = {
    message: string,
    status: string,
  };
  let decode = (json): response => {
    message: Json.Decode.(json |> field("message", string)),
    status: Json.Decode.(json |> field("status", string)),
  };
};

module RandomDogApi = ApiCall.Make(ApiConfig);

[@react.component]
let make = () => {
  let (apiState, fetch) =
    RandomDogApi.useApi(
      ~url="https://dog.ceo/api/breeds/image/random",
      ~headers={"Content-Type": "application/json"},
      (),
    );

  React.useEffect0(() => {
    let params: ApiConfig.params = {q: "dogs"};
    fetch(params);
    None;
  });

  switch (apiState) {
  | `Initial
  | `Loading => <div> {"Loading" |> ReasonReact.string} </div>
  | `Error(_) => <div> {"Something went wrong" |> ReasonReact.string} </div>
  | `Loaded(randomDog) => <img src={randomDog.message} />
  };
};
