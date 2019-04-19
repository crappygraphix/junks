export function getRoot() {
  let x = window.location.href.split("/")
  if(x.length >= 4 && x[3] == "rw") {
    return "rw"
  } else {
    return ""
  }
}
