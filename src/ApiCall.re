let buildUrl = (url, params) => {
  switch (params) {
  | None => url
  | Some(obj) => url ++ ApiCall__QueryString.stringify(obj)
  };
};

module type Config = {
  type response;
  let decode: Js.Json.t => response;
};

let (||=) = (value, defaultValue) =>
  switch (value) {
  | Some(v) => v
  | None => defaultValue
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

  let useApi = (~url, ~headers, ~method=`Get, ~body=?, ()) => {
    let (state, dispatch) = React.useReducer(reducer, `Initial);
    let fetch = queryParams => {
      dispatch(StartFetching);
      let _ =
        Js.Promise.(
          Fetch.fetchWithInit(
            buildUrl(url, queryParams),
            Fetch.RequestInit.make(
              ~method_={
                switch (method) {
                | `Get => Fetch.Get
                | `Post => Fetch.Post
                | `Patch => Fetch.Patch
                | `Delete => Fetch.Delete
                };
              },
              ~headers=Fetch.HeadersInit.make(headers),
              ~body=
                Fetch.BodyInit.make(
                  Js.Json.stringify(
                    Js.Json.object_(body ||= Js.Dict.empty()),
                  ),
                ),
              (),
            ),
          )
          |> then_(Fetch.Response.json)
          |> then_(json =>
               json
               |> decode
               |> (res => dispatch(FetchedSuccess(res)) |> resolve)
             )
          |> catch(_ => Js.Promise.resolve(dispatch(FetchedFailed)))
        );
      ();
    };

    (state, fetch);
  };
};
