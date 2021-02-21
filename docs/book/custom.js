const doubleTab = (text) => {
    return text.startsWith("        ")
};

const singleTab = (text) => {
    return text.startsWith("    ")
};

const trimStart = (text, amount) => {
    let lines = text.split(/[\n\r]/g)
    return lines.map(x => x.substring(amount)).join("\n")
};

document.querySelectorAll("pre code").forEach((element) => {
    let html = element.innerHTML;

    if (doubleTab(html)) {
        html = trimStart(html, 8)
    } else if (singleTab(html)) {
        html = trimStart(html, 4)
        console.log(html)
    }

    element.innerHTML = html;
}
)