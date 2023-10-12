Program ThreadTest;
{test multi threading capability }
{
OUTPUT
thread 1 started
thread 1 thri 0 Len(S)= 1
thread 1 thri 1 Len(S)= 2
thread 1 thri 2 Len(S)= 3
thread 1 thri 3 Len(S)= 4
thread 1 thri 4 Len(S)= 5
thread 1 thri 5 Len(S)= 6
thread 1 thri 6 Len(S)= 7
thread 1 thri 7 Len(S)= 8
thread 1 thri 8 Len(S)= 9
thread 1 thri 9 Len(S)= 10
thread 1 thri 10 Len(S)= 11
thread 1 thri 11 Len(S)= 12
thread 1 thri 12 Len(S)= 13
thread 1 thri 13 Len(S)= 14
thread 1 thri 14 Len(S)= 15
thread 2 started
thread 3 started
thread 1 thri 15 Len(S)= 16
thread 2 thri 0 Len(S)= 1
thread 3 thri 0 Len(S)= 1
thread 1 thri 16 Len(S)= 17
...
...
thread 5 thri 997 Len(S)= 998
thread 5 thri 998 Len(S)= 999
thread 5 thri 999 Len(S)= 1000
thread 5 finished
thread 10 thri 828 Len(S)= 829
thread 9 thri 675 Len(S)= 676
thread 4 thri 656 Len(S)= 657
thread 10 thri 829 Len(S)= 830
thread 9 thri 676 Len(S)= 677
thread 9 thri 677 Len(S)= 678
thread 10 thri 830 Len(S)= 831
thread 10 thri 831 Len(S)= 832
thread 10 thri 832 Len(S)= 833
thread 10 thri 833 Len(S)= 834
thread 10 thri 834 Len(S)= 835
thread 10 thri 835 Len(S)= 836
thread 10 thri 836 Len(S)= 837
thread 10 thri 837 Len(S)= 838
thread 10 thri 838 Len(S)= 839
thread 10 thri 839 Len(S)= 840
thread 9 thri 678 Len(S)= 679
...
...
thread 4 thri 994 Len(S)= 995
thread 4 thri 995 Len(S)= 996
thread 4 thri 996 Len(S)= 997
thread 4 thri 997 Len(S)= 998
thread 4 thri 998 Len(S)= 999
thread 4 thri 999 Len(S)= 1000
thread 4 finished
10

}

uses
{$ifdef unix}cthreads, {$endif} sysutils;

const
   threadcount = 10;
   stringlen = 1000;

var
   finished : longint;

threadvar
   thri : ptrint;

function f(p : pointer) : ptrint;
var
   s : ansistring;
begin
   Writeln('thread ',longint(p),' started');
   thri:=0;
   while (thri<stringlen) do begin
      s:=s+'1'; { create a delay }
      writeln('thread ',longint(p),' thri ',thri,' Len(S)= ',length(s));
      inc(thri);
   end;
   Writeln('thread ',longint(p),' finished');
   InterLockedIncrement(finished);
   f:=0;
end;


var
   i : longint;

Begin
   finished:=0;
   for i:=1 to threadcount do
      BeginThread(@f,pointer(i));
   while finished<threadcount do ;
      Writeln(finished);
End.
