T = 1 + Bless/(PlayerLv*100);
if(find_target() && scene_up())
{
     attack_target();
}
//wait(100,1);
//T2 = T;
//[Bless] = max(0.98, 0.9 + Bless/(PlayerLv*100)) * AttackFight;//获取祝福值