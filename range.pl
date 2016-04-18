range(Min,Max,Min).
range(Min,Max,Val) :- Val1 is Val-1,Val=<Max,range(Min,Max,Val1).