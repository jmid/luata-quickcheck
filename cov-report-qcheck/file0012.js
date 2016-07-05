var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 14</i>';
states['fold000001'] = false;
texts['fold000016'] = '<a href="javascript:fold(\'fold000016\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 16 to line 202</i>';
states['fold000016'] = false;
texts['fold000206'] = '<a href="javascript:fold(\'fold000206\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 206 to line 246</i>';
states['fold000206'] = false;
texts['fold000252'] = '<a href="javascript:fold(\'fold000252\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 252 to line 252</i>';
states['fold000252'] = false;
texts['fold000256'] = '<a href="javascript:fold(\'fold000256\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 256 to line 433</i>';
states['fold000256'] = false;
texts['fold000436'] = '<a href="javascript:fold(\'fold000436\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 436 to line 578</i>';
states['fold000436'] = false;
texts['fold000580'] = '<a href="javascript:fold(\'fold000580\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 580 to line 630</i>';
states['fold000580'] = false;
texts['fold000632'] = '<a href="javascript:fold(\'fold000632\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 632 to line 690</i>';
states['fold000632'] = false;
texts['fold000692'] = '<a href="javascript:fold(\'fold000692\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 692 to line 698</i>';
states['fold000692'] = false;
texts['fold000700'] = '<a href="javascript:fold(\'fold000700\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 700 to line 706</i>';
states['fold000700'] = false;
texts['fold000708'] = '<a href="javascript:fold(\'fold000708\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 708 to line 710</i>';
states['fold000708'] = false;
texts['fold000712'] = '<a href="javascript:fold(\'fold000712\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 712 to line 718</i>';
states['fold000712'] = false;
texts['fold000720'] = '<a href="javascript:fold(\'fold000720\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 720 to line 774</i>';
states['fold000720'] = false;
texts['fold000776'] = '<a href="javascript:fold(\'fold000776\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 776 to line 1213</i>';
states['fold000776'] = false;

function fold(id) {
  tmp = document.getElementById(id).innerHTML;
  document.getElementById(id).innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
