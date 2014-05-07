import Snap.Snaplet.Fay

data App = App { _fay :: Snaplet Fay }

makeLens ''App
