module collections {
    "use strict"
    // TODO:
    // high performance collection implementations

    export interface ILessFunction<T> {
	(a: T, b: T): boolean;
    }

    export interface IEqualsFunction<T> {
	(a: T, b: T): boolean;
    }

    export interface IHashFunction<T> {
	(a: T): number;
    }

    export interface IIterator<T> {
	next(): { value: T; done: boolean };
    }

    export interface IIterable<T> {
	iterator(): IIterator<T>;
    }

    function arrayIterator<T>(array: T[]) : IIterator<T> {
	var a = array.slice();
	var i = 0;
	return {
	    next: () =>
		{
		    var done = i === a.length;
		    var value : T;
		    if (!done) {
			value = a[i];
			i++;
		    }
		    return { value: value, done: done };
		}
	};
    }

    export class Queue<T> {
	private _values: T[] = [];

	get size() {
	    return this._values.length;
	}

	dequeue() {
	    return this._values.shift();
	}

	enqueue(v: T) {
	    this._values.push(v);
	}
    }

    export class Map<T, U> {
	private _keys: T[] = [];
	private _values: U[] = [];

	constructor(private equals: IEqualsFunction<T>,
		    private hash: IHashFunction<T>) { }

	private lookup(key: T) {
	    for (var i = 0; i < this._keys.length; i++)
		if (this.equals(key, this._keys[i]))
		    return i;

	    return -1;
	}

	get size() {
	    return this._keys.length;
	}

	clear() {
	    this._keys = [];
	    this._values = [];
	}

	delete(key: T) {
	    var idx = this.lookup(key);
	    if (idx === -1)
		return false;

	    this._keys.splice(idx, 1);
	    this._values.splice(idx, 1);
	    return true;
	}

	entries() : IIterator<{ key: T; value: U }>{
	    var keys = this._keys.slice();
	    var values = this._values.slice();
	    var i = 0;
	    return {
		next: () =>
		    {
			var done = i === keys.length;
			var value : { key: T; value: U };
			if (!done) {
			    value = { key: keys[i], value: values[i] };
			    i++;
			}
			return { value: value, done: done };
		    }
	    };
	}

	forEach(callback: (value: U, key: T, map: Map<T,U>) => void, thisArg?: any) {
	    // TODO: prevent changes during iteration
	    for (var i = 0; i < this._keys.length; i++)
		callback.call(thisArg, this._values[i], this._keys[i], this);
	}

	get(key: T) : U {
	    var idx = this.lookup(key);
	    if (idx === -1)
		return undefined;

	    return this._values[idx];
	}

	has(key: T) {
	    var idx = this.lookup(key);
	    return idx !== -1;
	}

	keys() {
	    return arrayIterator(this._keys);
	}

	set(key: T, value: U) {
	    var idx = this.lookup(key);
	    if (idx === -1) {
		this._keys.push(key);
		this._values.push(value);
	    } else {
		this._values[idx] = value;
	    }

	    return this;
	}

	values() {
	    return arrayIterator(this._values);
	}
    }

    function imul(a: number, b: number) {
	var ah  = (a >>> 16) & 0xffff;
	var al = a & 0xffff;
	var bh  = (b >>> 16) & 0xffff;
	var bl = b & 0xffff;
	// the shift by 0 fixes the sign on the high part
	// the final |0 converts the unsigned value into a signed value
	return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
    }

    export function setContentEquals<T>(a: Set<T>, b: Set<T>) {
	if (a.size !== b.size || a.currentHash !== b.currentHash)
	    return false;

	var iter = a.values();
	var v = iter.next();
	while (!v.done) {
	    if (!b.has(v.value))
		return false;
	    v = iter.next();
	}

	return true;
    }

    export function setHashFunction<T>(set: Set<T>) {
	var r = set.currentHash ^ 2166136261;
	return imul(r, 16777619);
    }

    export class Set<T> {
	private _values: T[] = [];
	public currentHash = 0;

	constructor(private equals: IEqualsFunction<T>,
		    private hash: IHashFunction<T>) { }

	private lookup(value: T) {
	    for (var i = 0; i < this._values.length; i++)
		if (this.equals(value, this._values[i]))
		    return i;

	    return -1;
	}

	get size() {
	    return this._values.length;
	}

	add(value: T) {
	    var h = this.hash(value);
	    var idx = this.lookup(value);
	    if (idx === -1) {
		this._values.push(value);
		this.currentHash ^= h;
	    }

	    return this;
	}

	clear() {
	    this._values = [];
	    this.currentHash = 0;
	}

	delete(value: T) {
	    var h = this.hash(value);
	    var idx = this.lookup(value);
	    if (idx === -1)
		return false;

	    this._values.splice(idx, 1);
	    this.currentHash ^= h;
	    return true;
	}

	entries() : IIterator<{ key: T; value: T }>{
	    var values = this._values.slice();
	    var i = 0;
	    return {
		next: () =>
		    {
			var done = i === values.length;
			var value : { key: T; value: T };
			if (!done) {
			    value = { key: values[i], value: values[i] };
			    i++;
			}
			return { value: value, done: done };
		    }
	    };
	}

	forEach(callback: (value: T, key: T, set: Set<T>) => void, thisArg?: any) {
	    // TODO: prevent changes during iteration
	    for (var i = 0; i < this._values.length; i++)
		callback.call(thisArg, this._values[i], this._values[i], this);
	}

	has(key: T) {
	    var idx = this.lookup(key);
	    return idx !== -1;
	}

	values() {
	    return arrayIterator(this._values);
	}

	toString() {
	    return "[ " + this._values.join(", ") + " ]";
	}
    }

    function swap<T>(a: T[], i: number, j: number) {
	var tmp = a[i];
	a[i] = a[j];
	a[j] = tmp;
    }

    export class BinaryHeap<T> {
	private values: T[] = [];

	constructor(private less: ILessFunction<T>) { }

	get size() {
	    return this.values.length;
	}

	dequeue() {
	    var r = this.values[0];
	    var n = this.size - 1;

	    this.values[0] = this.values[n];
	    this.values.pop();

	    var i = 0;
	    while (true) {
		var left = i << 1;
		var right = left + 1;

		if (right < n &&
		    this.less(this.values[right], this.values[i]) &&
		    this.less(this.values[right], this.values[left]))
		{
		    swap(this.values, i, right);
		    i = right;
		} else if (left < n &&
			   this.less(this.values[left], this.values[i]))
		{
		    swap(this.values, i, left);
		    i = left;
		} else {
		    return r;
		}
	    }
	}

	enqueue(v: T) {
	    var i = this.size;
	    var parent = i >>> 1;

	    this.values.push(v);

	    while (i > 0 && this.less(this.values[i], this.values[parent])) {
		swap(this.values, i, parent);
		i = parent;
		parent = i >>> 1;
	    }
	}
    }
}

module evreact {
    "use strict"

    var uid = 0;

    export interface IUniqueId {
	uid: number;
    }

    interface IPriority extends IUniqueId {
	priority: number;
    }

    function imul(a: number, b: number) {
	var ah  = (a >>> 16) & 0xffff;
	var al = a & 0xffff;
	var bh  = (b >>> 16) & 0xffff;
	var bl = b & 0xffff;
	// the shift by 0 fixes the sign on the high part
	// the final |0 converts the unsigned value into a signed value
	return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
    }

    function hashInt(a: number) {
	var r = a ^ 2166136261;
	return imul(r, 16777619);
    }

    function hashUniqueId(a: IUniqueId) {
	return hashInt(a.uid);
    }

    function identicalEquals<T>(a: T, b: T) {
	return a === b;
    }

    function comparePriority(a: IPriority, b: IPriority) {
	return a.priority < b.priority;
    }


    class PrioritySet<T extends IPriority> {
	private queue = new collections.BinaryHeap<T>(comparePriority);
	private set = new collections.Set<T>(identicalEquals, hashUniqueId);

	get size() {
	    return this.queue.size;
	}

	clear() {
	    if (this.size !== 0)
		throw "Illegal state"
	    this.set.clear();
	}

	dequeue() {
	    return this.queue.dequeue();
	}

	enqueue(v: T) {
	    if (!this.set.has(v)) {
		this.set.add(v);
		this.queue.enqueue(v);
	    }
	}
   }

    export interface IDisposable {
	dispose(): void;
    }

    export interface IHandler<T> extends IUniqueId {
	handleEvent(args: T): void;
    }

    export interface IEvent<T> extends IUniqueId {
	addHandler(handler: IHandler<T>): void;
	removeHandler(handler: IHandler<T>): void;
    }

    export interface ITriggerableEvent<T> extends IEvent<T> {
	trigger(e: T): void;
    }

    export module event {
	class SingleEventTarget<T> implements IEvent<T> {
	    public uid = uid++;
	    private listeners = new collections.Set<IHandler<T>>(identicalEquals, hashUniqueId);
	    private args: T;

	    constructor(private name?: string) { }

	    private triggerLoop(listener: IHandler<T>) {
		listener.handleEvent(this.args);
	    }

	    trigger(e: T) {
		this.args = e;
		this.listeners.forEach(this.triggerLoop, this);
		this.args = null;
	    }

	    addHandler(handler: IHandler<T>) {
		this.listeners.add(handler);
	    }

	    removeHandler(handler: IHandler<T>) {
		this.listeners.delete(handler);
	    }

	    toString() {
		return this.name + "[" + this.uid + "]";
	    }
	}

	// Workaround wrong EventTarget definition.
	// See https://typescript.codeplex.com/workitem/45
	export interface EventTarget {
	    removeEventListener(type: string, listener: { handleEvent: Function; }, useCapture?: boolean): void;
	    removeEventListener(type: string, listener: Function, useCapture?: boolean): void;
	    addEventListener(type: string, listener:  { handleEvent: Function; }, useCapture?: boolean): void;
	    addEventListener(type: string, listener: Function, useCapture?: boolean): void;
	    dispatchEvent(evt: Event): boolean;
	}

	class EventTargetWrapper implements IEvent<Event> {
	    public uid = uid++;
	    constructor(private target: EventTarget,
			private type: string) { }

	    addHandler(handler: IHandler<Event>) {
		this.target.addEventListener(this.type, handler);
	    }

	    removeHandler(handler: IHandler<Event>) {
		this.target.removeEventListener(this.type, handler);
	    }
	}

	export function create<T>(name?: string) : ITriggerableEvent<T> {
	    return new SingleEventTarget<T>(name);
	}

	export function wrap(target: EventTarget, type: string) : IEvent<Event> {
	    return new EventTargetWrapper(target, type);
	}
    }

    interface INotifiable<T> {
	notifyDeactivation(aux: number, args: T): void;
	notifyMatch(aux: number, args: T): void;
	notifyUnmatch(aux: number, args: T): void;
    }

    class SimpleNet<T> implements IDisposable, IUniqueId {
	public uid = uid++;

	private matching = false;

	public parent: INotifiable<T>;
	public aux = 0;

	constructor(public orch: SealedOrchestrator<T>,
		    public priority: number) { }

	setMatching(v: boolean, args:T) {
	    if (v)
		this.parent.notifyMatch(this.aux, args);
	    else if (this.matching)
		this.parent.notifyUnmatch(this.aux, args);
	    this.matching = v;
	}

	start(args:T) {
	    throw "Abstract method";
	}

	stop() {
	    throw "Abstract method";
	}

	dispose() {
	    this.stop();
	}
    }

    export interface Orchestrator<T> {
    }

    export interface DebugOrchestrator<T> extends Orchestrator<T> {
	isEmpty: boolean;
	onEvent: IEvent<IEvent<T>>;
	onStepBegin: IEvent<IEvent<T>>;
	onStepEnd: IEvent<IEvent<T>>;
    }

    class SealedOrchestrator<T> implements Orchestrator<T> {
	private evaluating = false;

	private dispatchers = new collections.Map<IEvent<T>, Dispatcher<T>>(identicalEquals, hashUniqueId);
	private muxers = new collections.Map<Set<IEvent<T>>, Muxer<T>>(collections.setContentEquals, collections.setHashFunction);

	private eventQueue = new collections.Queue<IEvent<T>>();
	private argsQueue = new collections.Queue<T>()
	private args: T;

	private activeGroundTerms = new collections.Set<GroundTermNet<T>>(identicalEquals, hashUniqueId);
	private activeOperators = new PrioritySet<OperatorNet<T>>();
	private disablingOperators = new PrioritySet<OperatorNet<T>>();
	private callbacks = new PrioritySet<CallbackNet<T>>();

	get isEmpty() {
	    return this.dispatchers.size == 0 && this.muxers.size == 0;
	}

	enqueueGroundTerm(net: GroundTermNet<T>) {
	    this.activeGroundTerms.add(net);
	}

	enqueueOpEval(net: OperatorNet<T>) {
	    this.activeOperators.enqueue(net);
	}

	enqueueNotifyDisable(net: OperatorNet<T>) {
	    this.disablingOperators.enqueue(net);
	}

	enqueueCallback(net: CallbackNet<T>) {
	    this.callbacks.enqueue(net);
	}

	dispatcher(event: IEvent<T>) {
	    var d = this.dispatchers.get(event);
	    if (d === undefined) {
		d = new Dispatcher<T>(this, event);
		this.dispatchers.set(event, d);
	    }
	    return d;
	}

	private muxer(events: collections.Set<IEvent<T>>) {
	    var m = this.muxers.get(events);
	    if (m === undefined) {
		m = new Muxer<T>(this, events);
		this.muxers.set(events, m);
	    }
	    return m;
	}

	subscribe(events: collections.Set<IEvent<T>>) {
	    return new Subscription(this.muxer(events));
	}

	unsubscribeDispatcher(event: IEvent<T>) {
	    this.dispatchers.delete(event);
	}

	unsubscribeMuxer(events: collections.Set<IEvent<T>>) {
	    this.muxers.delete(events);
	}

	enqueueEvent(event: IEvent<T>, args: T) {
	    if (this.evaluating) {
		this.eventQueue.enqueue(event);
		this.argsQueue.enqueue(args);
	    } else {
		this.evaluating = true;
		while (true) {
		    this.evalEvent(event, args);
		    if (this.eventQueue.size === 0) {
			this.evaluating = false;
			return;
		    }
		    event = this.eventQueue.dequeue();
		    args = this.argsQueue.dequeue();
		}
	    }
	}

	private evalEventLoop(net: GroundTermNet<T>) {
	    net.eval(this.args);
	}

	evalEvent(event: IEvent<T>, args: T) {
	    if (event !== null)
		this.dispatchers.get(event).evalEvent(args);

	    this.args = args;
	    this.activeGroundTerms.forEach(this.evalEventLoop, this);
	    this.activeGroundTerms.clear();
	    this.args = null;

	    while (this.activeOperators.size !== 0) {
		var net = this.activeOperators.dequeue();
		net.setMatching(net.isMatching(), args);
	    }
	    this.activeOperators.clear();

	    while (this.disablingOperators.size !== 0) {
		var net = this.disablingOperators.dequeue();
		if (net.active.size === 0)
		    net.parent.notifyDeactivation(net.aux, args);
	    }
	    this.disablingOperators.clear();

	    while (this.callbacks.size !== 0) {
		var callbacknet = this.callbacks.dequeue();
		callbacknet.cb(args);
	    }
	    this.callbacks.clear();
	}
    }

    class SealedDebugOrchestrator<T> extends SealedOrchestrator<T> implements DebugOrchestrator<T> {
	public onEvent = event.create<IEvent<T>>();
	public onStepBegin = event.create<IEvent<T>>();
	public onStepEnd = event.create<IEvent<T>>();
    }

    class Dispatcher<T> implements IUniqueId, IHandler<T> {
	public uid = uid++;

	private active = new collections.Set<Muxer<T>>(identicalEquals, hashUniqueId);
	private inactive = new collections.Set<Muxer<T>>(identicalEquals, hashUniqueId);
	private deactivate: Muxer<T>[];
	private args : T;

	constructor(private orch: SealedOrchestrator<T>,
		    private event: IEvent<T>) { }

	handleEvent(args: T) {
	    this.orch.enqueueEvent(this.event, args);
	}

	evalEventLoop(m: Muxer<T>) {
	    if (m.evalEvent(this, this.args))
		this.deactivate.push(m);
	}

	evalEvent(args: T) {
	    this.args = args;
	    this.deactivate = [];
	    this.active.forEach(this.evalEventLoop, this);
	    for (var i = 0; i < this.deactivate.length; i++) {
		this.active.delete(this.deactivate[i]);
		this.inactive.add(this.deactivate[i]);
	    }
	    this.deactivate = null;
	    this.args = null;
	    if (this.active.size === 0)
		this.event.removeHandler(this);
	}

	attach(mux: Muxer<T>) {
	    if (this.active.size === 0)
		this.event.addHandler(this);
	    this.inactive.delete(mux);
	    this.active.add(mux);
	}

	detach(mux: Muxer<T>) {
	    this.inactive.delete(mux);
	    this.active.delete(mux);
	    if (this.active.size === 0) {
		this.event.removeHandler(this);
		if (this.inactive.size === 0)
		    this.orch.unsubscribeDispatcher(this.event);
	    }
	}

	toString() {
	    return this.event.toString();
	}
    }

    class Muxer<T> implements IUniqueId {
	public uid = uid++;

	private activeSubscriptions = new collections.Set<Subscription<T>>(identicalEquals, hashUniqueId);
	private inactiveSubscriptions = new collections.Set<Subscription<T>>(identicalEquals, hashUniqueId);

	private enabledDispatchers = new collections.Set<Dispatcher<T>>(identicalEquals, hashUniqueId);
	private disabledDispatchers = new collections.Set<Dispatcher<T>>(identicalEquals, hashUniqueId);

	private args : T;

	private constructorLoop(e: IEvent<T>) {
	    this.disabledDispatchers.add(this.orch.dispatcher(e));
	}

	constructor(private orch: SealedOrchestrator<T>,
		    private events: collections.Set<IEvent<T>>) {
	    this.events.forEach(this.constructorLoop, this);
	}

	private evalEventLoop(s: Subscription<T>) {
	    s.evalEventFun(s.evalEventObj, this.args);
	}

	evalEvent(dispatcher: Dispatcher<T>, args: T) {
	    var r = this.activeSubscriptions.size === 0;
	    if (r) {
		this.enabledDispatchers.delete(dispatcher);
		this.disabledDispatchers.add(dispatcher);
	    } else {
		this.args = args;
		this.activeSubscriptions.forEach(this.evalEventLoop, this);
		this.args = null;
	    }
	    return r;
	}

	private enableLoop(d: Dispatcher<T>) {
	    d.attach(this);
	    this.enabledDispatchers.add(d);
	}

	enable(subscription: Subscription<T>) {
	    if (this.activeSubscriptions.size === 0) {
		this.disabledDispatchers.forEach(this.enableLoop, this);
		this.disabledDispatchers.clear();
	    }
	    this.inactiveSubscriptions.delete(subscription);
	    this.activeSubscriptions.add(subscription);
	}

	disable(subscription: Subscription<T>) {
	    this.activeSubscriptions.delete(subscription);
	    this.inactiveSubscriptions.add(subscription);
	}

	private unsubscribeLoop(d: Dispatcher<T>) {
	    d.detach(this);
	}

	unsubscribe(subscription: Subscription<T>) {
	    this.activeSubscriptions.delete(subscription);
	    this.inactiveSubscriptions.delete(subscription);
	    if (this.activeSubscriptions.size === 0 &&
		this.inactiveSubscriptions.size === 0)
	    {
		this.enabledDispatchers.forEach(this.unsubscribeLoop, this);
		this.disabledDispatchers.forEach(this.unsubscribeLoop, this);
		this.orch.unsubscribeMuxer(this.events);
	    }
	}

	toString() {
	    return this.events.toString();
	}
    }

    class Subscription<T> implements IUniqueId {
	public uid = uid++;
	public evalEventFun: (obj: any, e: T) => void; // should be (obj: GroundTermNet<T>, e: T) => void
	public evalEventObj: GroundTermNet<T>;

	constructor(private mux: Muxer<T>) { }

	enable() {
	    this.mux.enable(this);
	}

	disable() {
	    this.mux.disable(this);
	}

	dispose() {
	    this.mux.unsubscribe(this);
	}

	toString() {
	    return this.mux.toString();
	}
    }

    class UnaryOperatorNet<T> extends SimpleNet<T> implements INotifiable<T> {
	constructor(orch: SealedOrchestrator<T>,
		    public subnet: SimpleNet<T>) {
	    super(orch, 1 + subnet.priority);

	    subnet.parent = this;
	}

	start(args: T) {
	    this.subnet.start(args);
	}

	stop() {
	    this.subnet.stop();
	}

	notifyDeactivation(aux: number, args: T) {
	    this.parent.notifyDeactivation(this.aux, args);
	}

	notifyMatch (aux: number, args: T) {
	    this.setMatching(true, args);
	}

	notifyUnmatch(aux: number, args: T) {
	    this.setMatching(false, args);
	}
    }

    class CallbackNet<T> extends UnaryOperatorNet<T> {
	constructor(orch: SealedOrchestrator<T>,
		    subnet: SimpleNet<T>,
		    public cb: (e: T) => void) {
	    super(orch, subnet);
	}
    }

    class ReactNet<T> extends CallbackNet<T> {
	constructor(orch: SealedOrchestrator<T>,
		    subnet: SimpleNet<T>,
		    cb: (e: T) => void) {
	    super(orch, subnet, cb);
	}

	notifyMatch (aux: number, args: T) {
	    this.orch.enqueueCallback(this);
	    this.setMatching(true, args);
	}

	toString() {
	    return "(" + this.subnet + ") |-> ...";
	}
    }

    class FinallyNet<T> extends CallbackNet<T> {
	constructor(orch: SealedOrchestrator<T>,
		    subnet: SimpleNet<T>,
		    cb: (e: T) => void) {
	    super(orch, subnet, cb);
	}

	notifyDeactivation (aux: number, args: T) {
	    this.orch.enqueueCallback(this);
	}

	toString() {
	    return "(" + this.subnet + ") |=> ...";
	}
    }

    class IterNet<T> extends UnaryOperatorNet<T> {
	notifyMatch (aux: number, args: T) {
	    this.start(args);
	    this.setMatching(true, args);
	}

	toString() {
	    return "+(" + this.subnet + ")";
	}
    }

    class GroundTermNet<T> extends SimpleNet<T> {
	private active = false;
	private successful = false;

	private pos : Subscription<T>;
	private neg : Subscription<T>;

	constructor(orch: SealedOrchestrator<T>,
		    private predicate: (args: T) => boolean,
		    e: IEvent<T>,
		    bound: collections.Set<IEvent<T>>) {
	    super(orch, 0);
	    var eset = new collections.Set<IEvent<T>>(identicalEquals, hashUniqueId);
	    eset.add(e);
	    this.pos = orch.subscribe(eset);
	    this.neg = orch.subscribe(bound);
	    this.pos.evalEventFun = this.posCb;
	    this.neg.evalEventFun = this.negCb;
	    this.pos.evalEventObj = this;
	    this.neg.evalEventObj = this;
	}

	private posCb(o:GroundTermNet<T>, args: T) {
	    if (o.predicate(args)) {
		o.successful = true;
		o.orch.enqueueGroundTerm(o);
	    }
	}

	private negCb(o:GroundTermNet<T>, args: T) {
	    o.orch.enqueueGroundTerm(o);
	}

	private initialized() {
	    return this.pos !== null;
	}

	start (args: T) {
	    this.successful = false;
	    this.setMatching(this.successful, args);
	    if (!this.active) {
		this.pos.enable();
		this.neg.enable();
		this.active = true;
	    }
	}

	eval (args: T) {
	    this.active = false;
	    this.pos.disable();
	    this.neg.disable();
	    this.setMatching(this.successful, args);
	    if (!this.active)
		this.parent.notifyDeactivation(this.aux, args);
	}

	stop () {
	    if (this.initialized()) {
		this.pos.dispose();
		this.neg.dispose();
		this.pos = null;
		this.neg = null;
	    }
	}

	toString() {
	    var c = this.active ? "." : "";
            return c + this.pos + "/" + this.neg;
	}
    }

    function maxPriority<T>(nets: SimpleNet<T>[]) {
	var r = -1;

	for (var i = 0; i < nets.length; i++) {
	    var p = nets[i].priority;
	    if (r < p)
		r = p;
	}

	return r;
    }

    class OperatorNet<T> extends SimpleNet<T> implements INotifiable<T> {
	public active = new collections.Set<number>(identicalEquals, hashInt);

	constructor(orch: SealedOrchestrator<T>,
		    public subnets: SimpleNet<T>[]) {
	    super(orch, 1 + maxPriority(subnets));

	    for (var i = 0; i < subnets.length; i++) {
		var n = subnets[i];
		n.parent = this;
		n.aux = i;
	    }
	}

	substart(i: number, args: T) {
	    this.active.add(i);
	    this.subnets[i].start(args);
	}

	stop() {
	    for (var i = 0; i < this.subnets.length; i++)
		this.subnets[i].stop();
	}

	notifyDeactivation(aux: number, args: T) {
	    this.active.delete(aux);
	    if (this.active.size === 0)
		this.orch.enqueueNotifyDisable(this);
	}

	isMatching() : boolean {
	    throw "Abstract method";
	}

	notifyMatch(aux: number, args: T) {
	    throw "Abstract method";
	}

	notifyUnmatch(aux: number, args: T) {
	    throw "Abstract method";
	}
    }

    function opString<T>(operands:SimpleNet<T>[], op: string, empty: string) {
	if (operands.length === 0)
	    return empty;

	return "(" + operands.join(") " + op + " (") + ")";
    }

    class CatNet<T> extends OperatorNet<T> {
	private submatching = false;

	isMatching() {
	    return this.submatching;
	}

	start(args: T) {
	    if (this.subnets.length !== 0) {
		this.submatching = false;
		this.substart(0, args);
	    } else {
		this.parent.notifyDeactivation(this.aux, args);
	    }
	}

	notifyMatch(aux: number, args: T) {
	    var next = aux + 1;
	    if (next === this.subnets.length) {
		this.submatching = true;
		this.orch.enqueueOpEval(this);
	    } else {
		this.substart(next, args);
	    }
	}

	notifyUnmatch(aux: number) {
	    var next = aux + 1;
	    if (next === this.subnets.length) {
		this.submatching = false;
		this.orch.enqueueOpEval(this);
	    }
	}

	toString() {
	    return opString(this.subnets, "-", "nil");
	}
    }

    class CommutativeOperatorNet<T> extends OperatorNet<T> {
	public submatching = new collections.Set<number>(identicalEquals, hashInt);

	constructor(orch: SealedOrchestrator<T>,
		    subnets: SimpleNet<T>[]) {
		super(orch, subnets);
	}

	start(args: T) {
	    if (!this.active.has(-1)) {
		if (this.subnets.length === 0)
		    this.setMatching(this.isMatching(), args);
		else
		    for (var i = 0; i < this.subnets.length; i++)
			this.substart(i, args);

		this.notifyDeactivation(-1, args);
	    }
	}

	notifyMatch (aux: number) {
	    this.submatching.add(aux);
	    if (this.isMatching())
		this.orch.enqueueOpEval(this);
	}

	notifyUnmatch(aux: number) {
	    this.submatching.delete(aux);
	    if (!this.isMatching())
		this.orch.enqueueOpEval(this);
	}
    }

    class AllNet<T> extends CommutativeOperatorNet<T> {
	isMatching() {
	    return this.submatching.size === this.subnets.length;
	}

	toString() {
	    return opString(this.subnets, "&&&", "epsilon");
	}
    }

    class AnyNet<T> extends CommutativeOperatorNet<T> {
	isMatching() {
	    return this.submatching.size !== 0;
	}

	toString() {
	    return opString(this.subnets, "|||", "nil");
	}
    }

    export module orchestrator {
	export function create<T>() : Orchestrator<T> {
	    return new SealedOrchestrator<T>();
	}

	export function createDebug<T>() : DebugOrchestrator<T> {
	    return new SealedDebugOrchestrator<T>();
	}
    }

    export module expr {
	export interface Expr<T> {
	}

	interface SealedExpr<T> extends Expr<T> {
	    compile(orch: SealedOrchestrator<T>,
		    bounds: collections.Set<IEvent<T>>): SimpleNet<T>;
	}

	function compileAll<T>(orch: SealedOrchestrator<T>,
			       bounds: collections.Set<IEvent<T>>,
			       exprs:SealedExpr<T>[]) {
	    var nets = new Array<SimpleNet<T>>(exprs.length);
	    for (var i = 0; i < exprs.length; i++)
		nets[i] = exprs[i].compile(orch, bounds);

	    return nets;
	}

	class SimpleExpr<T> implements SealedExpr<T> {
	    constructor(private event: IEvent<T>,
			private pred: (args: T) => boolean) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new GroundTermNet<T>(orch, this.pred, this.event, bounds);
	    }
	}

	class AllExpr<T> implements SealedExpr<T> {
	    constructor(private subexprs: SealedExpr<T>[]) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new AllNet<T>(orch, compileAll(orch, bounds, this.subexprs));
	    }
	}

	class AnyExpr<T> implements SealedExpr<T> {
	    constructor(private subexprs: SealedExpr<T>[]) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new AnyNet<T>(orch, compileAll(orch, bounds, this.subexprs));
	    }
	}

	class CatExpr<T> implements SealedExpr<T> {
	    constructor(private subexprs: SealedExpr<T>[]) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new CatNet<T>(orch, compileAll(orch, bounds, this.subexprs));
	    }
	}

	class IterExpr<T> implements SealedExpr<T> {
	    constructor(private subexpr: SealedExpr<T>) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new IterNet<T>(orch, this.subexpr.compile(orch, bounds));
	    }
	}

	class ReactExpr<T> implements SealedExpr<T> {
	    constructor(private subexpr: SealedExpr<T>,
			private reaction: (e: T) => void) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new ReactNet<T>(orch, this.subexpr.compile(orch, bounds), this.reaction);
	    }
	}

	class FinallyExpr<T> implements SealedExpr<T> {
	    constructor(private subexpr: SealedExpr<T>,
			private reaction: (e: T) => void) { }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		return new FinallyNet<T>(orch, this.subexpr.compile(orch, bounds), this.reaction);
	    }
	}

	class RestrictExpr<T> implements SealedExpr<T> {
	    private tempBounds: collections.Set<IEvent<T>>;

	    constructor(private subexpr: SealedExpr<T>,
			private bounds: IEvent<T>[]) { }

	    compileLoop(e: IEvent<T>) {
		this.tempBounds.add(e);
	    }

	    compile(orch: SealedOrchestrator<T>, bounds: collections.Set<IEvent<T>>) {
		this.tempBounds = new collections.Set<IEvent<T>>(identicalEquals, hashUniqueId);

		for (var i = 0; i < this.bounds.length; i++)
		    this.tempBounds.add(this.bounds[i]);

		bounds.forEach(this.compileLoop, this);

		var net = this.subexpr.compile(orch, this.tempBounds);
		this.tempBounds = null;
		return net;
	    }
	}

	function trueP<T>(ignored: T) {
	    return true;
	}

	export function simple<T>(e: IEvent<T>) {
	    return cond(e, trueP);
	}

	export function cond<T>(e: IEvent<T>, pred: (args: T) => boolean): Expr<T> {
	    return new SimpleExpr(e, pred);
	}

	export function all<T>(subexprs: Expr<T>[]): Expr<T> {
	    return new AllExpr<T>(<SealedExpr<T>[]> <any> subexprs);
	}

	export function any<T>(subexprs: Expr<T>[]): Expr<T> {
	    return new AnyExpr<T>(<SealedExpr<T>[]> <any> subexprs);
	}

	export function cat<T>(subexprs: Expr<T>[]): Expr<T> {
	    return new CatExpr<T>(<SealedExpr<T>[]> <any> subexprs);
	}

	export function iter<T>(subexpr: Expr<T>): Expr<T> {
	    return new IterExpr<T>(<SealedExpr<T>> <any> subexpr);
	}

	export function restrict<T>(subexpr: Expr<T>, neg: IEvent<T>[]): Expr<T> {
	    return new RestrictExpr<T>(<SealedExpr<T>> <any> subexpr, neg);
	}

	export function react<T>(subexpr: Expr<T>, reaction: (args: T) => void): Expr<T> {
	    return new ReactExpr<T>(<SealedExpr<T>> <any> subexpr, reaction);
	}

	export function finallyDo<T>(subexpr: Expr<T>, reaction: (args: T) => void): Expr<T> {
	    return new FinallyExpr<T>(<SealedExpr<T>> <any> subexpr, reaction);
	}

	function ignoreAux(aux: number) {
	}

	export function start<T>(args: T, orch: Orchestrator<T>, expr: Expr<T>): IDisposable {
	    var iexpr = <SealedExpr<T>> expr;
	    var iorch = <SealedOrchestrator<T>> orch;
	    var net = iexpr.compile(iorch, new collections.Set<IEvent<T>>(identicalEquals, hashUniqueId));
	    net.parent = {
		notifyDeactivation: (aux: number) => net.stop(),
		notifyMatch: ignoreAux,
		notifyUnmatch: ignoreAux
	    };
	    net.aux = 0;
	    net.start(args);
	    return net;
	}

	export function stop(net: IDisposable) {
	    net.dispose();
	}
    }
}
