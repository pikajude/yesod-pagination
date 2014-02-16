### yesod-pagination

(`yesod-paginate` is taken)

This package makes it pretty simple to paginate records.

You'll probably want to use the function `paginateWith`, which takes a function `t -> SqlQuery (SqlExpr (Entity r))` and gives you `Handler (Page r)`. The returned `Page` object might have next page and previous page links (pre-rendered).

The parameter to `paginateWith` should be an `esqueleto` query. The most trivial example is `return` (`paginate` is basically `paginateWith return`). This one is a bit less trivial.

``` haskell
import Import
import Yesod.Paginate

getMessagesR :: Handler Html
getMessagesR = do
    myMessages <- paginateWith $ \msg -> do
        orderBy [asc (msg ^. MsgCreatedAt)]
        return msg

    defaultLayout $(widgetFile "message-list")
```

For a lesson on how to write `esqueleto` queries, see [the haddocks](http://hackage.haskell.org/package/esqueleto-1.3.4.5/docs/Database-Esqueleto.html).
