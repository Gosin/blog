---
title: Brew Package Graph
date: 2018-04-08 19:55:07
tags:

---

## 想法

`Homebrew` 安装各种包时，会同时安装各种依赖，有些包会共享同一些依赖。我就好奇他们之间到底是怎样联系起来的，`brew deps —-tree <package name> ` 可以用来显示一个包的所有依赖。但是我想看的是所有已安装包的依赖。这个命令就不够用了， 所以就用Mathematica写了一个函数来做这个事儿。

## 实现

### 收集依赖信息

shell脚本收集依赖信息到一个文本文件。

```bash
brewpacks="$(brew list)"
for pack in ${brewpacks}
do
	brew deps --tree $pack >> brewdeps.txt
done
```

### 构造依赖图

Mathematica 函数处理数据并生成图（最开始逻辑没想清楚，还走了一些弯路~）

```mma
Clear[BrewPackageGraph];
BrewPackageGraph[filePath_String] := Module[
    {
        (* {0, Global} is a dummy head to represent global environment. *)
        dependencyChain = {{0, Global}},
        lines,
        result
    },

    lines = StringSplit[Import[filePath], "\n"];
    result = Reap[
        Function[{line},
            If [line === "",
                (* Reset dependency chain. *)
                dependencyChain = {{0, Global}}
                ,
                With[{packName = StringCases[line, (WordCharacter|"-")..] // First},
                    (* Collect vertices. *)
                    Sow[packName, Vertex];
                    With[{indentation = StringPosition[line, packName][[1, 1]]},
                        With[{dependencyPos = FirstPosition[dependencyChain, {indentation, _}]},
                            dependencyChain = If [MissingQ[dependencyPos],
                                (* Add package to the dependency chain. *)
                                Append[dependencyChain, {indentation, packName}]
                                ,
                                ReplacePart[
                                    (* Go back to upper dependency, drop the rest dependencies. *)
                                    Take[dependencyChain, First[dependencyPos]],
                                    (* Replace same level dependency with new package. *)
                                    First[dependencyPos] -> {indentation, packName}
                                ]
                            ];
                            (* Collect dependency edge. *)
                            Sow[
                                Last[SelectFirst[Reverse[dependencyChain], First[#] < indentation &]] -> packName,
                                Edge
                            ]
                        ]
                    ]
                ]
            ]
        ]  /@ lines
    ];

    Graph[
        (* Vertices list. *)
        DeleteDuplicates[result[[2, 1]]],
        (* Edges list. *)
        DeleteDuplicates[DeleteCases[result[[2, 2]], Global -> _]],
        VertexLabels -> All
    ]
]
```

## 成果

{% asset_img BrewPackageGraph.png  Brew Package Graph %}



找出所有已安装的包（忽略依赖）

```mma
Select[VertexList[brewGraph], VertexInDegree[brewGraph, #] === 0 &]

===

{"ack", "boost", "clisp", "cmake", "cscope", "emacs", "gcc", "gdb", "git", "gnupg", "guile", "node", "postgresql", "rust", "tldr", "tmux", "tree", "vim"}
```

被依赖最多的包（三甲）

```mma
Take[
	Sort[
		Select[VertexList[brewGraph], VertexInDegree[brewGraph, #] > 0 &], 
  		VertexInDegree[brewGraph, #1] > VertexInDegree[brewGraph, #2] &
	],
	3
]

=== 

{"gmp", "openssl", "readline"}
```

当然还可以做其它各种分析，还在玩弄中
