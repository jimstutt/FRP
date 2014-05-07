Prelude> data D = D Int
Prelude> let f (D i) = "ok"
Prelude> f undefined
"*** Exception: Prelude.undefined
Prelude> newtype D = D Int
Prelude> let f (D i) = "ok"
Prelude> f undefined
"ok"