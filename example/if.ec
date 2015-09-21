T1 = 3;
T2 = 4;
elici_io::output("T2:~w", [T2]);
if(T1 > T2)
{
    T2 = 3*4;
}
else
{
   T2 = 3 + 4;
}
elici_io::output("T2:~w", [T2]);
