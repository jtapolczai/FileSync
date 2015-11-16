module Debug.Trace.Disable where

trace _ x = x
traceShow = trace
traceM _ = return ()
