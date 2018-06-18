Project name:Foundations of the C++Concurrency Memory Model
Team:姜也东PB15111628
abstract：以论文Foundations of the C++Concurrency Memory Model为中心的文献调研，通过阅读该文并从中发现疑惑点与该兴趣的方面展开扩展调研。调研时间以空闲时间为主。
Project progress report：已查阅相关资料。
Issues：主要有3个问题，均关于trylock
      1.trylock即使在lock可用时也可能失败，但并未给出具体情况。
      2.下文中的fail是否特指lock可用时。
      3.trylock失败不显示信息意义不明。
Ideas：1.可以考虑细化memory location来解决adjacent bitfields问题。
       2.以安全性为目标的模型作为扩展项。
