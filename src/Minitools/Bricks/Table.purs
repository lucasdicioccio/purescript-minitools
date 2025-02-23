-- A brick for making interactive and dynamic HTML tables.
--
-- This bricks is a somewhat powerful table.
-- A table contains items along columns, that is rows represent a collection of information of same nature whereas column project items across many different dimensions.
-- The table header will have two levels, one with column names and one with column contents (e.g., to contain buttons)
--
-- the Props type is parameterized by action, slot, effect monad and type of item
-- the component defines a Column type
-- Column defines how to display a given item in a given dimension, it contains
-- - a `name` which serve as title in the table header
-- - a `colgrp` array of properties as table colgroups (e.g., to add classes or spans)
-- - a `column` sub-component used as table sub-header
-- - a `cell` function mapping an item into a sub-component used as the table cell
--
-- the Props type has
-- - an `items` array of items (corresponding to rows in the table)
-- - a `template` field with an array defining every Column
-- - a `rowClass` function allow to specialize the HTML class of a given row
--
-- The rendering lays-out every row by applying the colums in the same order as the template and by applying the rows in the same order as the items. That is, ordering and filtering should be done outside of this table.
--
-- Helpers allow to render the table header or table body separately.
module Minitools.Bricks.Table
  ( render
  , Props
  , Column
  , renderTableHeader
  , renderTableBody
  ) where

import Prelude (($), map)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed (HTMLcol) as DOM
import DOM.HTML.Indexed.ScopeValue (ScopeValue(..)) as DOM

type Column a s m item =
  { name :: String
  , cell :: item -> HH.ComponentHTML a s m
  , colgrp :: Array (HP.IProp DOM.HTMLcol a)
  , column :: HH.ComponentHTML a s m
  }

type Props a s m item =
  { template :: Array (Column a s m item)
  , rowClass :: item -> Array HH.ClassName
  , items :: Array item
  }

render :: forall a s m item. Props a s m item -> HH.ComponentHTML a s m
render props =
    HH.table
    [ HP.class_ $ HH.ClassName "table"
    ]
    [ renderTableColGroup props.template
    , renderTableHeader props.template
    , renderTableBody props.rowClass props.template props.items
    ]

renderTableColGroup
  :: forall a s m r.
  Array 
    { colgrp :: Array (HP.IProp DOM.HTMLcol a)
    | r
    }
  -> HH.ComponentHTML a s m
renderTableColGroup template =
  let
    colgrp x = HH.col x.colgrp
  in
  HH.colgroup_
  $ map colgrp template

renderTableHeader
  :: forall a s m r.
  Array 
    { name :: String
    , column :: HH.ComponentHTML a s m
    | r
    }
  -> HH.ComponentHTML a s m
renderTableHeader template =
  let
    th x = HH.th [ HP.scope DOM.ScopeCol, HP.title x.name ] [ HH.text x.name ]
    colth x = HH.th_ [ x.column ]
  in
  HH.thead_
  [ HH.tr_
    $ map th template
  , HH.tr_
    $ map colth template
  ]

renderTableBody :: forall a s m item r. (item -> Array HH.ClassName) -> Array {cell :: item -> HH.ComponentHTML a s m |r} -> Array item -> HH.ComponentHTML a s m
renderTableBody getClass template items =
  let
    cell item x = HH.td_ [ x.cell item ]
    row tpl item = HH.tr [ HP.classes $ getClass item] $ map (cell item) tpl
  in
  HH.tbody_
  $ map (row template) items
