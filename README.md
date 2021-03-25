# purescript-VexceptT

A version of [`ExceptT`](https://pursuit.purescript.org/packages/purescript-transformers/docs/Control.Monad.Except.Trans#t:ExceptT) that uses [`Veither`](https://pursuit.purescript.org/packages/purescript-veither/docs/Data.Veither) instead of [`Either`](https://pursuit.purescript.org/packages/purescript-either/docs/Data.Either#t:Either).

Inspired by and only slightly different from [`ExceptV`](https://pursuit.purescript.org/packages/purescript-checked-exceptions/3.1.0/docs/Control.Monad.Except.Checked#t:ExceptV)

```purescript
-- # Main types compared
newtype ExceptV  e m a = ExceptV (m (Either (Variant e) a))
newtype VexceptT e m a = ExceptV (m (Veither         e  a))

-- # Underlying types compared
data Either l r 
    = Left l 
    | Right r

newtype Veither e r = Veither (Variant ("_" :: a | e))

-- pseudo-code
type Variant ("_" :: a) = {type : "_", value: a }

-- # Comparing how errors are stored at runtim
ExceptV  (pure (Left    (inj _sym error))) :: ExceptV  e m a
VexceptT (pure (Veither (inj _sym error))  :: VexceptT e m a

-- Dropping the `Either` removes one layer of boxing at runtime
pure (Left {type: "error", value: e})) :: ExceptV  e m a
pure (     {type: "error", value: e})  :: VexceptT e m a

-- # Comparing how success value are stored at runtime
ExceptV  (pure (Right                 a)  :: ExceptV  e m a
VexceptT (pure (Veither (inj _sym error)) :: VexceptT e m a

-- Dropping the `Either` removes one layer of boxing at runtime
pure (Right              a)) :: ExceptV  e m a
pure ({type: "_", value: a}) :: VexceptT e m a
```