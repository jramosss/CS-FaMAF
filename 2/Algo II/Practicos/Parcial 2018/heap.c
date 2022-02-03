type heap = tuple
elems: array[1...n] of elem
size: nat
end

fun left(i:nat) ret j:nat
j:= 2*i
end

fun right(i:nat) ret j:nat
j:= 2*i+1
end

fun parent(i:nat) ret j:nat
j:= i ÷ 2
end

{Pre: 1 ≤i≤ h.size}
fun has_children(h:heap, i:nat) ret b:bool
b:= (left(i) ≤ h.size)
end
{Post: b = i tiene hijos en h}

fun has_parent(i:nat) ret b:bool
b:= (i 6= 1)
end

{Pre: 1 ≤ i ≤ h.size ∧ has_children(h,i)}
fun max_child(h:heap, i:nat) ret j:nat
if right(i) ≤ h.size ∧ h.elems[left(i)] ≤ h.elems[right(i)] then j:= right(i)
else j:= left(i)
fi
end
{Post: j = posición donde se encuentra el mayor de los hijos de i en h}

{Pre: 1 ≤ i ≤ h.size ∧ has_parent(i)}
proc lift(in/out h:heap,in i:nat)
swap(h.elems,i,parent(i))
end

{Pre: 1 ≤ i ≤ h.size ∧ has_parent(i)}
fun must_lift(h:heap, i:nat) ret b:bool
b:= (h.elems[i] > h.elems[parent(i)])
end
{Post: b = i es mayor que su padre}

{Pre: h (= H) es heap excepto tal vez porque el elem en h.elems[h.size] es grande}
proc float(in/out h:heap)
var c: nat
c:= h.size
while has_parent(c) ∧ must_lift(h,c) do
lift(h,c)
c:= parent(c)
od
end
{Post: h es un heap con los mismos elementos que H}

{Pre: h (= H) es heap excepto tal vez porque el elem en 1 es chico}
proc sink(in/out h:heap)
var p: nat
p:= 1
while has_children(h,p) ∧ must_lift(h,max_child(h,p)) do
p:= max_child(h,p)
lift(h,p)
od
end
{Post: h es un heap con los mismos elementos que H}