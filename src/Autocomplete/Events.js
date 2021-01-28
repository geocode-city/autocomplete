exports.customEvent = function (name, data) {
  return new CustomEvent(name, { detail: data });
}
