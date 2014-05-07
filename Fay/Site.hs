import Snap.Snaplet.Fay

routes = [("/fay", with fay fayServe)]

app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application." Nothing $ do
  fay' <- nestSnaplet "fay" fay initFay
  return $ App { _fay = fay' }