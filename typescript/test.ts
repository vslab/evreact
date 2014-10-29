///<reference path="evreact.ts"/>

function assert(t: boolean) {
    if (!t) {
	console.trace();
	throw "Assertion failed";
    }
}

function stringEquals(a: string, b: string) {
	return a == b;
}

function stringHash(s: string) {
	var h = 0;
	for (var i = 0; i < s.length; i++)
	    h = h * 33 + s.charCodeAt(i);

	return h;
}

function prepare(patterns:string[]) {
	var events = new collections.Map<string,evreact.ITriggerableEvent<number>>(stringEquals, stringHash);
	for (var i = 0; i < patterns.length; i++) {
	    for (var j = 0; j < patterns[i].length; j++) {
		var v = patterns[i][j];
		if (v.match(/[a-z]/i) && !events.has(v)) {
		    events.set(v, evreact.event.create<number>(v));
		}
	    }
	}

	var check = (expr:evreact.expr.Expr<number>) => {
	    for (var i = 0; i < patterns.length; i++) {
		var matchCount = 0;
		var receivedArgs = 1000;
		var update = (args:number) => {
		    matchCount++;
		    receivedArgs = args;
		}

		var orch = evreact.orchestrator.createDebug<number>();
		var expectedMatchCount = 0;
		var expectedArgs = 1000;
		var sentArgs = expectedArgs + 1;
		var expectedEmpty = false;
		var net = evreact.expr.start(sentArgs, orch, evreact.expr.react(expr, update));
		var logString = net.toString();

		for (var j = 0; j < patterns[i].length; j++) {
		    var v = patterns[i][j];
		    // 0 indicates net termination, with no match
		    // . indicates match without net termination
		    // ! indicates match and net termination together
		    // letters indicate event triggers

		    if (v.match(/[0!]/))
			expectedEmpty = true;

		    if (v.match(/[.!]/)) {
			expectedArgs = sentArgs;
			expectedMatchCount++;
		    }

		    assert(expectedEmpty == orch.isEmpty);
		    assert(expectedMatchCount == matchCount);
		    assert(expectedArgs == receivedArgs);

		    if (!v.match(/[0.!]/)) {
			sentArgs++;
			events.get(v).trigger(sentArgs);
		    }

		}

		evreact.expr.stop(net);
		assert(orch.isEmpty);
		console.log(logString);
	    }
	}

	var getevent = (v:string) => <evreact.IEvent<number>> events.get(v);

	return { getevent:getevent, check: check };
}

var p = prepare([ "a!", "bbba!bbb" ]);
p.check(evreact.expr.simple(p.getevent("a")));

var p = prepare([ "ab!", "bbbab!", "bbbcccaaacccaaacccb!aaabbbccc" ])
p.check(evreact.expr.cat([evreact.expr.simple(p.getevent("a")), evreact.expr.simple(p.getevent("b"))]));

var p = prepare([ "ab!", "ba!", "cccaaacccaaacccb!aaabbbccc" ])
p.check(evreact.expr.all([evreact.expr.simple(p.getevent("a")), evreact.expr.simple(p.getevent("b"))]));

var p = prepare([ "ab!", "ba!", "cccaaacccaaacccb!aaabbbccc" ])
p.check(evreact.expr.all([evreact.expr.simple(p.getevent("b")), evreact.expr.simple(p.getevent("a"))]));

var p = prepare([ "a.b!", "b.a!", "ccca.aacccaaacccb!aaabbbccc" ])
p.check(evreact.expr.any([evreact.expr.simple(p.getevent("a")), evreact.expr.simple(p.getevent("b"))]));

var p = prepare([ "a.b!", "b.a!", "ccca.aacccaaacccb!aaabbbccc" ])
p.check(evreact.expr.any([evreact.expr.simple(p.getevent("b")), evreact.expr.simple(p.getevent("a"))]));
