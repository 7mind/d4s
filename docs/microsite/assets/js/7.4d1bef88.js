(window.webpackJsonp=window.webpackJsonp||[]).push([[7],{353:function(t,a,s){"use strict";s.r(a);var e=s(42),n=Object(e.a)({},(function(){var t=this,a=t.$createElement,s=t._self._c||a;return s("ContentSlotsDistributor",{attrs:{"slot-key":t.$parent.slotKey}},[s("h1",{attrs:{id:"getting-started"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#getting-started"}},[t._v("#")]),t._v(" Getting Started")]),t._v(" "),s("p",[s("strong",[t._v("d4s")]),t._v(" is a Scala library that allows you to work with DynamoDB in a pure functional way.\nIt's powered by "),s("a",{attrs:{href:"https://izumi.7mind.io/latest/release/doc/index.html",target:"_blank",rel:"noopener noreferrer"}},[t._v("Izumi"),s("OutboundLink")],1),t._v(", uses Bifunctor IO and allows you to choose whatever effect type you want to use.\nIt provides flexible and extensible DSL, supports AWS SDK v2 and has great integration with "),s("a",{attrs:{href:"https://zio.dev/",target:"_blank",rel:"noopener noreferrer"}},[t._v("ZIO"),s("OutboundLink")],1),t._v(" and "),s("a",{attrs:{href:"https://bio.monix.io/",target:"_blank",rel:"noopener noreferrer"}},[t._v("Monix-BIO"),s("OutboundLink")],1),t._v(" (not yet but soon).")]),t._v(" "),s("h2",{attrs:{id:"dependencies"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#dependencies"}},[t._v("#")]),t._v(" Dependencies")]),t._v(" "),s("p",[t._v("To use "),s("code",[t._v("d4s")]),t._v(", add the following line in your "),s("code",[t._v("build.sbt")]),t._v(" file:")]),t._v(" "),s("div",{staticClass:"language- extra-class"},[s("pre",{pre:!0,attrs:{class:"language-text"}},[s("code",[t._v('libraryDependencies += "net.playq" %% "d4s" %% "1.0.13"\n')])])]),s("p",[t._v("The following modules are optional:")]),t._v(" "),s("p",[t._v("In case you want to have Circe codecs you should also add this:")]),t._v(" "),s("div",{staticClass:"language- extra-class"},[s("pre",{pre:!0,attrs:{class:"language-text"}},[s("code",[t._v('libraryDependencies += "net.playq" %% "d4s-circe" %% "1.0.13"\n')])])]),s("p",[t._v("If you want to use the metrics package from d4s you can add it like this:")]),t._v(" "),s("div",{staticClass:"language- extra-class"},[s("pre",{pre:!0,attrs:{class:"language-text"}},[s("code",[t._v('libraryDependencies += "net.playq" %% "metrics" %% "1.0.10"\n')])])]),s("h2",{attrs:{id:"a-simple-example"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#a-simple-example"}},[t._v("#")]),t._v(" A simple example")]),t._v(" "),s("p",[t._v("Let's imagine that we have the following interface for a game's ladders repository:")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("trait")]),t._v(" Ladder"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("_"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" _"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("def")]),t._v(" submitScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("userId"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" UUID"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" score"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Long")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("DynamoException"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Unit")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("def")]),t._v(" getScores"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("DynamoException"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" List"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UserWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("We need to provide table definition first, e.g. table name, hash key, range key (if exists), etc.\nTo do this the one should extend "),s("code",[t._v("TableDef")]),t._v(" trait and pass "),s("code",[t._v("DynamoMeta")]),t._v(" which is required by "),s("code",[t._v("TableDDL")])]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" LadderTable"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("implicit")]),t._v(" meta"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" DynamoMeta"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" TableDef "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" mainKey "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" DynamoKey"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("hashKey "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" DynamoField"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UUID"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"userId"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("override")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" table"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" TableReference "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" TableReference"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"d4s-ladder-table"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" mainKey"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("override")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" ddl"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" TableDDL "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" TableDDL"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("table"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("We mustn't forget  to  provide codecs for our custom type that we wanna store in the DB.\nHopefully, d4s has capabilities to automatically derive codes from user's defined types.")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("object")]),t._v(" LadderTable "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" UserIdWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("userId"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" UUID"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" score"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Long")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("object")]),t._v(" UserIdWithScore "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n    "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("implicit")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" codec"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" D4SCodec"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UserIdWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" D4SCodec"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("derived"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UserIdWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("By default, d4s relies on "),s("a",{attrs:{href:"https://propensive.com/opensource/magnolia/",target:"_blank",rel:"noopener noreferrer"}},[t._v("Magnolia"),s("OutboundLink")],1),t._v(" to derive typeclasses,\nbut you could also use "),s("a",{attrs:{href:"https://circe.github.io/circe/",target:"_blank",rel:"noopener noreferrer"}},[t._v("Circe"),s("OutboundLink")],1),t._v(" to do the same. In case you wanna use "),s("code",[t._v("circe")]),t._v(" just\ninclude "),s("code",[t._v("d4s-circe")]),t._v(" module as a dependency for your project.")]),t._v(" "),s("p",[t._v("Finally, we can write some queries!\nTo build a query we need "),s("code",[t._v("DynamoConnector")]),t._v(" and of course our previously defined table.\n"),s("code",[t._v("DynamoConnector")]),t._v(" is meant to execute a query.")]),t._v(" "),s("p",[t._v("In order to get all scores from the ladder we need to scan the whole table. The "),s("code",[t._v("execPagedFlatten")]),t._v(" combinator\nhandles pagination and flattens the result to one dimensional list.")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[t._v("connector"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("run"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"get scores query"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  table"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("scan"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("decodeItems"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UserIdWithScoreStored"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("execPagedFlatten"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("We also wanna update our game ladder with new scores. Here is how we can easily do this:")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[t._v(" connector"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("run"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"submit user\'s score"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  table"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("updateItem"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("UserIdWithScoreStored"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("userId"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("value"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" score"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("value"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("Full implementation of "),s("code",[t._v("Ladder[_]")]),t._v("  could look like this:")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" D4SLadder"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("+")]),t._v("_"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("+")]),t._v("_"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" BIO"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("connector"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" DynamoConnector"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" ladderTable"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" LadderTable"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" Ladder"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("ladderTable"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("_\n\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("override")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("def")]),t._v(" getScores"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("DynamoException"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" List"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UserWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n    connector\n      "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("run"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"get scores query"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n        table"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("scan"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("decodeItems"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("UserIdWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("execPagedFlatten"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n      "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("override")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("def")]),t._v(" submitScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("userId"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" UUID"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" score"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Long")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" F"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("DynamoException"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Unit")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n    connector\n      "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("run"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"submit user\'s score"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n        table"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("updateItem"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("UserIdWithScore"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("userId"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" score"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n      "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("void\n  "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("In case you wanna have a deeper look on project with d4s, you could play with it using this showcase project: "),s("a",{attrs:{href:"https://github.com/VladPodilnyk/d4s-example",target:"_blank",rel:"noopener noreferrer"}},[t._v("d4s-example"),s("OutboundLink")],1)])])}),[],!1,null,null,null);a.default=n.exports}}]);