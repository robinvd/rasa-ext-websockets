# rasa-ext-websockets
websocket extension for Rasa

![](http://i.imgur.com/fGVui3A.gif)

# Install
add this to your stack.yaml

```
packages:
        - '.'
        - location:
            git: https://github.com/robinvd/rasa-ext-websockets
            commit: 890916e0902091a6716f151e8f698d79dfa1f99a
          extra-dep: true
```

add this to your config file

```

import Rasa.Websockets

...

main = rasa $ do

  ...

  networkServer
```

now you can use the "Send" event listner and the "Message" event

function used in the example gif:

```
main = do
  ...
  onBufTextChanged upload

...

upload bufRef _ = do
   bufDo_ bufRef $ getText >>= dispatchEvent . SendAll . Y.toText

```
