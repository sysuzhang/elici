-module(mod).
//这是模块,不是脚本

function main()
{
     main(0, []);

}
function main(Argc, Argv)
{
    for(I = 0; I < argc; I++)
    {

          elici_io:format("arg:~p", [Argv[Argc]]);
    }
}