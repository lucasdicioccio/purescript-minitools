minitools
=========

A set of "mini" tools and utilities for writing web-applications with
Purescript-Halogen.

This set of utilities and 'bricks' is extracted from [PostgREST Table](https://dicioccio.fr/postgrest-table.html) and [Prometheus Monitor](https://dicioccio.fr/prometheus-monitor.html).
For charts, I use the already-open-sourced [Halogen-ECharts-Simple](https://github.com/lucasdicioccio/purescript-halogen-echarts-simple).

The philosophy behind these minitools is to offer composable libraries instead
of a whole framework. If you need more architectural guidance, please check out my [series of articles about how I write PureScript/Halogen apps](https://dicioccio.fr/topics/purescript-halogen-architecture.html).

__Utility types__

For API and distributed systems:
- Minitools.Api.Base: a Remote type
- Minitools.Seqnum: typed monotonic sequence numbers

For logging analytics (I'll document the PageEvents and Reports modules later):
- Minitools.Tracer: a contravariant-tracing type
- Minitools.PageEvents: a way to funnel DOM-events with annotations found on the page
- Minitools.Reports: a way to export anonymous recordings to some API sink

Misc:
- Minitools.BackgroundLoop: a timer-ticker based on Halogen.Subscription

__Bricks__

Bricks are (mostly) providing an HTML-layout for [Bulma.css](https://bulma.io/)
Do not act surprised if one day I move them under a "Bulma" submodule.

Only listing those that are non-Bulma specific or require some thought:

- Minitools.Bricks.ColorDot: some helper for inline-CSS "color dots"
- Minitools.Bricks.Emojis: just naming some emojis, I like emojis as cheap icons
- Minitools.Bricks.Table: a somewhat powerful Table object which is highly customizable, this brick naturally render items as _rows_ and individual features of items as columns; if you need a transposed version, I'd suggest you get inspiration from the code (and maybe contribute a bew brick); the table is backed by the `table` html component
