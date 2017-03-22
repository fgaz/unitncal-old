// Declaration
var course, year, subject;
var arrayidsubjects = [];
var link = "http://unitncal.fgaz.me/multical", hl = "c=";
/*var data= // example of input data structure
{
  "courses":{ //getCourseNames
    "1234":"Informatica"
  },
  "subjects":{
    "1234-1":[["1111","Analisi 1"],["2222","Algebra Lineare"],["3333","Prog 1"]],
    "1234-2":[["4444","Esame1 2"],["5555","e22"],["6666","e23"]]
  }
};*/

function load(){
  fillcourses();
  document.getElementById("submit").disabled = true;
}

function fillcourses(){
  var select = document.getElementById("course");
  var option;
  for (var id in data["courses"]) {
    option = document.createElement("option");
    option.text = data["courses"][id];
    option.value = id;
    select.add(option);
  }
}

function fillsubjects(){
  var select = document.getElementById("subject");
  var option;
  for (var id in data["subjects"]) {
    if (id == (course + "-" + year)){
      for (var i = 0; i < data["subjects"][id].length; i++) {
        option = document.createElement("option");
        option.value = data["subjects"][id][i][0];
        option.text = data["subjects"][id][i][1];
        select.add(option);
      }
    }
  }
}

function resetsubjects() {
  var select = document.getElementById("subject");
  while (select.length > 1){
    select.remove(1);
  }
  document.getElementById("defaultoptionsubject").selected = "true";
}

// functions called when a select status change
function updatecourse(){
  document.getElementById("cancel").disabled = false;
  var select = document.getElementById("course");
  var i = select.selectedIndex;
  course = select.options[i].value;
  resetsubjects();
  fillsubjects();
  document.getElementById("submit").disabled = false;
}

function updateyear(){
  document.getElementById("cancel").disabled = false;
  var select = document.getElementById("year");
  var i = select.selectedIndex;
  year = select.options[i].value;
  resetsubjects();
  fillsubjects();
  document.getElementById("submit").disabled = false;
}

function updatesubject(){
  document.getElementById("cancel").disabled = false;
  var select = document.getElementById("subject");
  var i = select.selectedIndex;
  subject = select.options[i].value;
  document.getElementById("submit").disabled = false;
}

// function called when the user click on submit button
function submit(){
  document.getElementById("submit").disabled = true;
  var ok = false; // check if the user has chosen correctly the three selects
  if (typeof course != "undefined" && typeof year != "undefined" && typeof subject != "undefined")
    ok = true;

  var find = false; // check if the subject id already exists in the arrayidsubjects
  for (var i = 0; i < arrayidsubjects.length; i++)
    if (arrayidsubjects[i] == subject)
      find = true;

  if ( (!find) && ok ){
    arrayidsubjects.push(subject);
    var s = "";
    for (var i = 0; i < arrayidsubjects.length; i++){
      if (i == 0)
        s = "?"+ hl + arrayidsubjects[i];
      else
        s = s + "&" + hl + arrayidsubjects[i];
    }

    document.getElementById("link").value = link + s;
  }
}

// function called when pressed the cancel button
function cancel(){
  document.getElementById("defaultoptioncourse").selected = true;
  document.getElementById("defaultoptionyear").selected = true;
  document.getElementById("defaultoptionsubject").selected = true;

  resetsubjects();

  arrayidsubjects = [];
  document.getElementById("link").value = "";

  document.getElementById("cancel").disabled = true;
}
