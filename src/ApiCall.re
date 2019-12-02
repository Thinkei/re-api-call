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

  let useApi = (~url, ~headers, ~method=`Get, ()) => {
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
