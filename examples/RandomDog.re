module ApiConfig = {
  type response = {
    message: string,
    status: string,
  };
  let decode = (json): response => {
    message: Json.Decode.(json |> field("message", string)),
    status: Json.Decode.(json |> field("status", string)),
  };
  let url = "https://dog.ceo/api/breeds/image/random";
  let fetchMethod = Fetch.Get;
};

module RandomDogApi = ApiCall.Make(ApiConfig);

[@react.component]
let make = () => {
  let apiState = RandomDogApi.useApi(`NotSpecified);
  switch (apiState) {
  | `Initial
  | `Loading => <div> {"Loading" |> ReasonReact.string} </div>
  | `Error => <div> {"Something went wrong" |> ReasonReact.string} </div>
  | `Loaded(randomDog) => <img src={randomDog.message} />
  };
};
