function onFileChange(evt){
    var x = document.getElementById ("myfile");
    var file = evt.target.files[0];
    if (file) {
        var r = new FileReader();
        r.onload = function(e) {
                     var contents = e.target.result;
                     orig_editor.setValue(contents);
                   };
        r.readAsText(file);
    }
}

document.getElementById('fileinput').addEventListener('change', onFileChange, false);

// Registered helper accumulates a list of 'lint'-like hints
CodeMirror.registerHelper("lint", "lua", function(text) {
    var obj = parsePPrint(text);
//  console.log("lint helper: ", obj, obj.errors);
    var line = obj.errors.line - 1;
    var column = obj.errors.column - 1;
    var found = [];
    found.push({ from:     CodeMirror.Pos(line, column),
		 to:       CodeMirror.Pos(line, column + 1),
		 message:  obj.errors.msg,
		 severity: "error"
	       });
    console.log("warnings: ", obj.warnings);
    var warnings = obj.warnings;
    for (num in warnings) {
	var warning = warnings[num];
	console.log("warning: ", warning);
	found.push({ from:     CodeMirror.Pos(warning.line - 1, 1),
		     to:       CodeMirror.Pos(warning.line - 1, 1),
		     message:  warning.msg,
		     severity: "warning"
		   });
    };
    return found;
});

var heaps = {};     // global string array
var label_map = {}; // global int array
var cache = {};     // global dom/html array

CodeMirror.registerHelper("textHover", "lua", function(cm, data, node) {
    var line = cm.coordsChar({left : node.clientX, top : node.clientY }, "window").line;
    var html;

    if (cache[line]) {
	cm.setCursor({ line: line, ch: 0});
	return cache[line]; // already in cache? don't recompute
    }

    html = '<h4>Abstract state at line ' + line
    	     + ' label ' + label_map[line] + '</h4><hr>';  // (token is null)

    if (data) {
        var token = data.token;
        line = data.pos.line;
        html = '<h4>Abstract state at line ' + line + ' label ' + label_map[line]
	         + '</h4><hr>';
    }

    cm.setCursor({ line: line, ch: 0});

    var heap = heaps[label_map[line]];
    if (typeof heap == 'string' || heap instanceof String) {
        html += '<pre>' + heap + '</pre>';
    }; //else {
//          html += '<table>';
//          html +=  '<tr><td>store: </td><td><pre>' + heap.store + '</pre></td></tr>';
//          html +=  '<tr><td>env:   </td><td><pre>' + heap.env   + '</pre></td></tr>';
//          html += '</table>';
//        }
    var result = document.createElement('div');
    result.innerHTML = html;
    cache[line] = result; // record in cache
    return result;
});

var orig_editor = CodeMirror.fromTextArea(document.getElementById("original"), {
    mode            : 'text/x-lua',
    theme           : "eclipse",
//  readOnly        : "nocursor",
    lineNumbers     : true,
    firstLineNumber : 0,
    gutters         : ["CodeMirror-lint-markers","CodeMirror-linenumbers"],
    lint            : true
//  textHover       : true
});

var internal_editor = CodeMirror.fromTextArea(document.getElementById("internal"), {
    mode            : 'text/x-lua',
    theme           : "eclipse",
    styleActiveLine : true,
    readOnly        : "nocursor",
    lineNumbers     : true,
    firstLineNumber : 0,
    gutters         : ["CodeMirror-linenumbers"],
    textHover       : true
});

function analyze() {
    var btn = document.getElementById('analyze')
    btn.disabled = true;
    btn.textContent = "Analyzing ..."

    var myWorker = new Worker("worker.js");

    myWorker.postMessage(orig_editor.getValue());
    myWorker.onmessage = function(e) {
        obj = e.data;
        console.log("errors is ", obj.errors);

        if (obj.last != null) {
            internal_editor.setValue(obj.last);
            heaps = heapsAsStrArray(obj.heaps);
            label_map = obj.label_map;
            cache = {};
        } else {
            console.log("no update, since no last");
        }
        btn.disabled = false;
        btn.textContent = "Analyze"
        console.log('Message received from worker');
    }
}

var abutton = document.getElementById('analyze');
abutton.addEventListener('click', analyze);

var simple = 'x = 1\n'
           + 'y = x + "foo"\n'
           + 'print(y)';

var fac = 'function fac (n)\n'
        + '   if n == 0\n'
        + '   then\n'
        + '      return 1\n'
        + '   else\n'
        + '      return n * fac(n-1)\n'
        + '   end\n'
        + 'end\n'
        + '\n'
        + 'local r = fac(5)\n'
        + 'print(r)\n';

var record = 'function mktable(f)\n'
	   + '   return { x = f("x"), y = f("y") }\n'
           + 'end\n'
           + '\n'
           + 'mktable(function (z) return z.."component" end)\n';

var seterror = '--  Set example from "Programming in Lua", p.129\n'
             + '\n'
             + 'Set = {}\n'
             + '\n'
             + 'local mt = {} -- metatable for sets\n'
             + '\n'
             + '-- create a new set with the values of a given list\n'
             + 'function Set.new (l)\n'
             + '   local set = {}\n'
             + '   setmetatable(set,mt)\n'
             + '   for _, v in ipairs(l) do set[v] = true end\n'
             + '   return set\n'
             + 'end\n'
             + '\n'
             + 'function Set.union (a, b)\n'
             + '   local res = Set.new{}\n'
             + '   for k in pairs(a) do res[k] = true end\n'
             + '   for k in pairs(b) do res[k] = true end\n'
             + '   return res\n'
             + 'end\n'
             + '\n'
             + '--mt.__add = Set.union  --- forgot to install handler\n'
             + '\n'
             + 's1 = Set.new{10,20,30,50}\n'
             + 's2 = Set.new{30,1}\n'
             + 's3 = s1 + s2\n';

function progSelect() {
    var pselect = document.getElementById('prog-select');
    var choice = pselect.value;
    if (choice == 'simple')      { orig_editor.setValue(simple); }
    else if (choice == 'fac')    { orig_editor.setValue(fac); }
    else if (choice == 'record') { orig_editor.setValue(record); }
    else                         { orig_editor.setValue(seterror); }

    //    console.log("Selected options ", pselect.options, " index ", pselect.selectedIndex);
    console.log("Selected value ", pselect.value);
//    console.log("Text ", pselect.options[pselect.selectedIndex].text);
};


var pselect = document.getElementById('prog-select');
//pselect.onchange = progSelect;
pselect.addEventListener('change', progSelect);
