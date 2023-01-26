1、Double类型的属性会报错，文件暂时保存在bak目录
2、
```haskell
d1 `set` [#value := 100]
```
属性变化没有信号emit，而
`(toGValue (100 :: CInt)) >>= (objectSetProperty d1 "value")`
是可以的