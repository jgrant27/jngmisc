with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Pi is

  a, b, c, d, e, g: Integer;
  f: array (0 .. 10000) of Integer;

procedure Dec(n : in out Integer) is
begin
  n := n - 1;
end Dec;

procedure Print_Digits(i : integer) is
s : String(1 .. 4);
begin
  put(s, i);
  for i in 1 .. s'Length loop
    if s(i) = ' ' then s(i) := '0';
    end if;
  end loop;
  put(s);
end Print_Digits;

begin
  a := 10000; b := 0; c := 10000; e := 0;
  for b in 0 .. c loop f(b) := a / 5; end loop;
  while c > 0 loop
    d := 0; g := c*2; b := c;
    while b > 0 loop
      d := d + f(b) * a; Dec(g); f(b) := d mod g;
      d := d / g; Dec(g); Dec(b);
      if b > 0 then d := d * b; end if;
    end loop;
    c := c - 14;
    Print_Digits(e + d / a);
    e := d mod a;
  end loop;
end;
