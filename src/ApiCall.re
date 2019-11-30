module type Config = {
  type response;
  let decode: Js.Json.t => response;

  let url: string;
  let fetchMethod: Fetch.requestMethod;
};

module Make = (C: Config) => {
  open C;
  type state = [ | `Initial | `Loading | `Error | `Loaded(response)];

  type action =
    | StartFetching
    | FetchedSuccess(response)
    | FetchedFailed;

  let reducer = (_, action: action) =>
    switch (action) {
    | StartFetching => `Loading
    | FetchedFailed => `Error
    | FetchedSuccess(response) => `Loaded(response)
    };

  let useApi = (authenBy) => {
    let (state, dispatch) = React.useReducer(reducer, `Initial);

    React.useEffect0(() => {
      dispatch(StartFetching);

      let headers =
        switch (authenBy) {
        | `SameOrigin =>
          Fetch.HeadersInit.make({
            "Content-Type": "application/json",
            "mode": "cors",
            "credentials": "include",
          })
        | `Jwt(jwtToken) =>
          Fetch.HeadersInit.make({
            "Content-Type": "application/json",
            "mode": "cors",
            "Jwt-Token": jwtToken,
          })
        | `NotSpecified =>
          Fetch.HeadersInit.make({"Content-Type": "application/json"})
        };
      let _ =
        Js.Promise.(
          Fetch.fetchWithInit(
            url,
            Fetch.RequestInit.make(~method_=fetchMethod, ~headers, ()),
          )
          |> then_(Fetch.Response.json)
          |> then_(json =>
               json
               |> decode
               |> (res => dispatch(FetchedSuccess(res)) |> resolve)
             )
          |> catch(_ => Js.Promise.resolve(dispatch(FetchedFailed)))
        );
      None;
    });
    state;
  };
};
