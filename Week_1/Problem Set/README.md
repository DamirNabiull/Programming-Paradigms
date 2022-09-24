# Problem Set 1 - Damir Nabiullin
## Task 1
1. *λx.(λy.xy) x*
1. *λx.(λx.x) x*
    - > λx.(λy.y) x
1. *λx.λy.x y*
1. *λx.x (λx.x)* 
    - > λx.x (λy.y)
1. *λx.(λx.x) x*
    - > λx.(λy.y) x
1. *(λx.λy.y) z x*
    - > (λw.λy.y) z x

## Task 2
1. *(λx.λy.x) y z*
    - > (λx.λw.x) y z 
    - > λw.y z 
    - > y
1. *(λx.λy.x) (λz.y) z w*
    - > (λx.λu.x) (λs.y) z w
    - > λu.λs.y z w
    - > λs.y w
    - > y
1.  *(λb.λf.λt.b t f) (λf.λt.t)*
    - > (λb.λf.λt.b t f) (λx.λy.y)
    - > λf.λt.(λx.λy.y) t f
    - > λf.λt.(λy.y) f
    - > λf.λt.f
1. *(λs.λz.s (s z)) (λb.λf.λt.b t f) (λf.λt.t)*
    - > (λs.λz.s (s z)) (λb.λf.λt.b t f) (λx.λy.y)
    - > (λz.(λb.λf.λt.b t f) (λb'.λf'.λt'.b' t' f' z)) (λx.λy.y)
    - > (λb.λf.λt.b t f) (λb'.λf'.λt'.b' t' f' (λx.λy.y))
    - > (λf.λt.(λb'.λf'.λt'.b' t' f'(λx.λy.y)) t f)
    - > (λf.λt.(λf'.λt'.(λx.λy.y) t' f') t f)
    - > (λf.λt.(λf'.λt'.(λy.y) f') t f)
    - > (λf.λt.(λf'.λt'.f') t f)
    - > (λf.λt.(λt'.t) f)
    - > λf.λt.t
1. *(λs.λz.s (s z)) (λs.λz.s (s z)) (λb.λf.λt.b t f) (λf.λt.t)*
    - > (λs.λz.s (s z)) (λs'.λz'.s' (s' z')) (λb.λf.λt.b t f) (λf'.λt'.t')
    - > (λz.(λs'.λz'.s' (s' z')) ((λs''.λz''.s'' (s'' z'')) z)) (λb.λf.λt.b t f) (λf'.λt'.t')
    - > (λs'.λz'.s' (s' z')) ((λs''.λz''.s'' (s'' z'')) (λb.λf.λt.b t f)) (λf'.λt'.t')
    - > (λz'.((λs''.λz''.s'' (s'' z'')) (λb.λf.λt.b t f)) (((λs''.λz''.s'' (s'' z'')) (λb''.λf''.λt''.b'' t'' f'')) z')) (λf'.λt'.t')
    - > (λs''.λz''.s'' (s'' z'')) (λb.λf.λt.b t f) (((λs''.λz''.s'' (s'' z'')) (λb''.λf''.λt''.b'' t'' f'')) (λf'.λt'.t'))
    - > Substitute from 4-th lambda equation
    - > (λs''.λz''.s'' (s'' z'')) (λb.λf.λt.b t f) (λf''.λt''.t'')
    - > Substitute from 4-th lambda equation
    - > λf.λt.t

## Task 3
tru = λt.λf.t

fls = λt.λf.f

test = λc.λt.λf.c t f

1. implies = λa.λb.(test a b tru)
1. implies fls tru
    - > (λa.λb.(test a b tru)) fls tru
    - > (test fls tru tru)
    - > (λc.λt.λf.c t f) fls tru tru
    - > (fls tru tru)
    - > (λt.λf.f) tru tru
    - > tru

## Task 4
c0 = λs.λz.z

c1 = λs.λz.sz

c2 = λs.λz.s (s z)

c3 = λs.λz.s (s (s z))

1.  > (2n + 1) = λn.λs.λz.n s (n s (s z))

    > (n<sup>2</sup> + 1) = λn.λs.λz.n (n s) (s z)

    > (2<sup>n</sup> + 1) = λn.λs.λz.n c<sub>2</sub> s (s z)

    > (2<sup>n+1</sup>) = λn.λs.λz.n c<sub>2</sub> s (n c<sub>2</sub> s z)

1.  2n + 1:
    - > (λn.λs.λz.n s (n s (s z))) (λs'.λz'.s' (s' z'))
    - > λs.λz.(λs''.λz''.s'' (s'' z'')) s ((λs'.λz'.s' (s' z')) s (s z))
    - > λs.λz.(λz''.s (s z'')) ((λs'.λz'.s' (s' z')) s (s z))
    - > λs.λz.s (s (λs'.λz'.s' (s' z')) s (s z))
    - > λs.λz.s (s (λz'.s (s z')) (s z))
    - > λs.λz.s (s (s (s (s z))))
    - > c<sub>5</sub>

    (n<sup>2</sup> + 1)
    - > (λn.λs.λz.n (n s) (s z)) (λs'.λz'.s' (s' z'))
    - > λs.λz.(λs''.λz''.s'' (s'' z'')) ((λs'.λz'.s' (s' z')) s) (s z)
    - > λs.λz.(λz''.((λs'.λz'.s' (s' z')) s) (((λs'.λz'.s' (s' z')) s) z'')) (s z)
    - > λs.λz.((λs'.λz'.s' (s' z')) s) (((λs'.λz'.s' (s' z')) s) (s z))
    - > λs.λz.(λz'.s (s z')) (((λs'.λz'.s' (s' z')) s) (s z))
    - > λs.λz.s (s (((λs'.λz'.s' (s' z')) s) (s z)))
    - > λs.λz.s (s ((λz'.s (s z')) (s z)))
    - > λs.λz.s (s (s (s (s z))))
    - > c<sub>5</sub>

    (2<sup>n</sup> + 1)
    - > (λn.λs.λz.n c<sub>2</sub> s (s z)) (c<sub>2</sub>)
    - > λs.λz.c<sub>2</sub> c<sub>2</sub> s (s z)
    - > λs.λz.(λs'.λz'.s' (s' z')) c<sub>2</sub> s (s z)
    - > λs.λz.(c<sub>2</sub> (c<sub>2</sub> s)) (s z)
    - > λs.λz.(λs'.λz'.s' (s' z') (λs''.λz''.s'' (s'' z'') s)) (s z)
    - > λs.λz.((λs''.λz''.s'' (s'' z'') s) ((λs''.λz''.s'' (s'' z'') s) (s z)))
    - > λs.λz.((λs''.λz''.s'' (s'' z'') s) (s (s (s z))))
    - > λs.λz.s (s (s (s (s z))))
    - > c<sub>5</sub>

    (2<sup>n+1</sup>)
    - > (λn.λs.λz.n c<sub>2</sub> s (n c<sub>2</sub> s z)) (c<sub>2</sub>)
    - > λs.λz.c<sub>2</sub> c<sub>2</sub> s (c<sub>2</sub> c<sub>2</sub> s z)
    - > λs.λz.(c<sub>2</sub> (c<sub>2</sub> s)) (c<sub>2</sub> c<sub>2</sub> s z)
    - > λs.λz.(c<sub>2</sub> s) ((c<sub>2</sub> s) (c<sub>2</sub> c<sub>2</sub> s z))
    - > λs.λz.(s (s ((c<sub>2</sub> s) (c<sub>2</sub> c<sub>2</sub> s z))))
    - > λs.λz.s (s (s (s (c<sub>2</sub> c<sub>2</sub> s z))))
    - > λs.λz.s (s (s (s ((c<sub>2</sub> (c<sub>2</sub> s)) z))))
    - > λs.λz.s (s (s (s ((c<sub>2</sub> s) ((c<sub>2</sub> s) z)))))
    - > λs.λz.s (s (s (s (s (s ((c<sub>2</sub> s) z))))))
    - > λs.λz.s (s (s (s (s (s (s (s z)))))))
    - > c<sub>8</sub>