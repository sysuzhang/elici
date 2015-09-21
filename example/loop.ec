for(I=1; I < 10; I++)
{
    Sum += Sum + i;
}
elici_io::output("Sum :~p", [Sum]);

I = 0;
while( I < 10)
{
  I = I + 1;
  elici_io::output("While:~p", [I]);
}