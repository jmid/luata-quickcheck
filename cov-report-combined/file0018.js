var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 297</i>';
states['fold000001'] = false;
texts['fold000299'] = '<a href="javascript:fold(\'fold000299\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 299 to line 354</i>';
states['fold000299'] = false;
texts['fold000357'] = '<a href="javascript:fold(\'fold000357\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 357 to line 489</i>';
states['fold000357'] = false;
texts['fold000491'] = '<a href="javascript:fold(\'fold000491\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 491 to line 541</i>';
states['fold000491'] = false;
texts['fold000543'] = '<a href="javascript:fold(\'fold000543\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 543 to line 589</i>';
states['fold000543'] = false;
texts['fold000591'] = '<a href="javascript:fold(\'fold000591\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 591 to line 596</i>';
states['fold000591'] = false;
texts['fold000598'] = '<a href="javascript:fold(\'fold000598\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 598 to line 604</i>';
states['fold000598'] = false;
texts['fold000606'] = '<a href="javascript:fold(\'fold000606\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 606 to line 612</i>';
states['fold000606'] = false;
texts['fold000614'] = '<a href="javascript:fold(\'fold000614\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 614 to line 666</i>';
states['fold000614'] = false;
texts['fold000668'] = '<a href="javascript:fold(\'fold000668\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 668 to line 1077</i>';
states['fold000668'] = false;

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
