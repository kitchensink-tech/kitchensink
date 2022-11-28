(function(){
  const copyText = (raw) => () => {
    const elt = document.createElement("input");
    elt.value = raw;
    elt.select();
    elt.setSelectionRange(0, 99999);
    navigator.clipboard.writeText(elt.value);
  };
  const xs = [... document.getElementsByTagName("code")];
  xs.filter((x) => {
    return x.classList[0] !== undefined;
  }).forEach((x) => {
    const raw = x.innerText ?? '';
    const div = document.createElement("div");
    const btn = document.createElement("button");
    btn.innerHTML = "📋️ copy snippet";
    btn.addEventListener("click", copyText(raw));
    btn.className = "btn btn-copy";
    div.appendChild(btn);
    x.parentNode.prepend(div);
  });
})()
