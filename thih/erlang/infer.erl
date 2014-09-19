% // 11 Type Inference
% object Infer {
%   import Pred._
%   import Assump._
%   import TIMonad._
% 
%   type Infer[E, T] = Ti => ClassEnv => List[Assump] => E => (List[Pred], T)
% }
% 
