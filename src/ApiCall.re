let (||=) = (value, defaultValue) =>
  switch (value) {
  | Some(v) => v
  | None => defaultValue
  };

let mapOpt = (fn, value) =>
  switch (value) {
  | None => None
  | Some(v) => fn(v)
  };

module type Config = {
  type params;
  let encode: params => Js.Json.t;

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
    let fetch = (queryParams: params) => {
      dispatch(StartFetching);

      let fullUrl =
        switch (method) {
        | `Post
        | `Patch
        | `Delete => url
        | `Get => url ++ "?" ++ (queryParams |> encode |> ApiCall__QueryString.stringify)
        };

      Js.log(fullUrl);
      let body =
        switch (method) {
        | `Post
        | `Patch
        | `Delete => Some(queryParams |> encode |> Js.Json.stringify)
        | `Get => None
        };

      let _ =
        Js.Promise.(
          Fetch.fetchWithInit(
            fullUrl,
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
              ~body=Fetch.BodyInit.make(body ||= ""),
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
