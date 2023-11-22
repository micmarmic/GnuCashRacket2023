function goToDate() {

    var protocol = location.protocol;
    var slashes = protocol.concat("//");
    var host = slashes.concat(window.location.host)
    const base_url = @|form-url|;
    
    var year = document.getElementById("inputYear").value;
    var month = document.getElementById("inputMonth").value;
    var day = document.getElementById("inputDay").value;
    
    var newURL = `${host}${base_url}${year}-${pad(month, 2)}-${pad(day, 2)}`;
    alert(newURL);
    location.replace(newURL);
}

function pad(num, size) {
    num = num.toString();
    while (num.length < size) num = "0" + num;
    return num;
}

function resetDays() {
    var select = document.getElementById('inputDay');
    var options = select.getElementsByTagName('option');
    /* remove all days */    
    for (var i=options.length; i--;) {
        select.removeChild(options[i]);
    }

    var year = document.getElementById("inputYear").value;
    var month = document.getElementById("inputMonth").value;
    var dt = new Date(year, month, 0); // day 0 == last day prev month
    var numDays = dt.getDate(); // getDay is day of week!!!

    alert(numDays);

    /* add days */
    for (var i = 0; i<numDays; i++){
        var opt = document.createElement('option');
        var day = i + 1;
        opt.value = day;
        opt.innerHTML = day;
        if(day ===  numDays) {
            opt.selected = true
        }
        select.appendChild(opt);        
    }
}