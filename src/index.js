import("./Main.elm").then(({ Elm }) => {
  var node = document.getElementById("main")
  var app = Elm.Main.init({ node: node })
})
