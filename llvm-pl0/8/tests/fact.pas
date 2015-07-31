program factorial;

function fact (x : integer) : integer;
begin
  if x = 0 then 1
  else x*fact(x-1)
end;

begin
  writeln('fact(10) = ',fact(10))
end.
