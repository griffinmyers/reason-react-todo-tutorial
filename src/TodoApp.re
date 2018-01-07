type item = {
  id: int,
  title: string,
  completed: bool
};

let valueFromEvent = (evt) : string => (
  evt
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

let str = ReasonReact.stringToElement;

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item, ~onToggle, _children) => {
    ...component,
    render: (_self) => {
      let checked = Js.Boolean.to_js_boolean(item.completed);

      <div className="item" onClick=((_) => onToggle())>
        <input _type="checkbox" checked />
        (str(item.title))
      </div>
    }
  };
};

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) => {
      <input
        value=text
        _type="text"
        placeholder="Write something to do"
        onChange=(reduce((evt) => valueFromEvent(evt)))
        onKeyDown=((evt) =>
          if (ReactEventRe.Keyboard.key(evt) == "Enter") {
            onSubmit(text);
            (reduce(() => ""))()
          }
        )
      />
    }
  };
};

type state = {
  count: int,
  items: list(item)
};

type action =
  | Tick
  | AddItem(string)
  | ToggleItem(int);

let component = ReasonReact.reducerComponent("TodoApp");

let lastId = ref(0);
let newItem = (text) => {
  lastId := lastId^ + 1;
  {id: lastId^, title: text, completed: true}
};

let pluralize = (l: list('a), name: string) => {
  switch (l) {
  | [] => name ++ "s"
  | [_head] => name
  | [_fst, _snd, ..._rest] => name ++ "s"
  }
};

let make = (_children) => {
  ...component,
  initialState: () => {
    count: 0,
    items: [
      {id: 0, title: "Write some things to do", completed: false}
    ]
  },
  reducer: (action, state) => {
    switch action {
    | Tick => ReasonReact.Update({...state, count: state.count + 1})
    | AddItem(text) => ReasonReact.Update({...state, items: [newItem(text), ...state.items]})
    | ToggleItem(id) =>
      let items = List.map((item) => {
        item.id === id ? { ...item, completed: !item.completed } : item;
      }, state.items);
      ReasonReact.Update({...state, items: items});
    }
  },
  didMount: ({reduce}) => {
    Js.Global.setInterval(reduce(() => Tick), 1000) |> ignore;
    ReasonReact.NoUpdate;
  },
  render: ({state: {items, count}, reduce}) => {
    let numItems = List.length(items);
    <div className="app">
      <div className="title">
        (str("What to do"))
        <Input onSubmit=(reduce((text) => AddItem(text))) />
      </div>
      <div className="items">(
        List.map((item) => {
          let key = string_of_int(item.id);
          <TodoItem item onToggle=(reduce(() => ToggleItem(item.id))) key />
        }, items)
          |> Array.of_list
          |> ReasonReact.arrayToElement
      )</div>
      <div className="footer">
        (str(string_of_int(numItems) ++ " " ++ pluralize(items, "item")))
      </div>
      <div className="footer">
        (str(string_of_int(count)))
      </div>
    </div>
  }
};
