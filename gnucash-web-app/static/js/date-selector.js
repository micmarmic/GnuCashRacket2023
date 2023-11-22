function goToDate() {

    var protocol = location.protocol;
    var slashes = protocol.concat("//");
    var host = slashes.concat(window.location.host)
    const base_url = @|form-url|;
    
    var y = document.getElementById("inputYear");
    var year = y.value;
    var m = document.getElementById("inputMonth");
    var month = m.value;
    var d = document.getElementById("inputDay");
    var day = d.value;
    
    var newURL = `${host}${base_url}${year}-${pad(month, 2)}-${pad(day, 2)}`;
    alert(newURL);
    location.replace(newURL);
}

function pad(num, size) {
    num = num.toString();
    while (num.length < size) num = "0" + num;
    return num;
}
