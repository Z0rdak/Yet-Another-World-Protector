package de.z0rdak.yawp.core.flag;

import net.minecraft.network.chat.contents.TranslatableContents;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum RegionFlag {

    BREAK_BLOCKS(new BooleanFlag("break_blocks", false)),
    SCOOP_FLUIDS(new BooleanFlag("scoop_fluids", false)),
    BREAK_ENTITIES(new BooleanFlag("break_entities", false)),
    PLACE_BLOCKS(new BooleanFlag("place_blocks", false)),
    PLACE_FLUIDS(new BooleanFlag("place_fluids", false)),
    ENTITY_PLACE(new BooleanFlag("entity-place", false)),
    EXPLOSION_ENTITY(new BooleanFlag("explosions-entities", false)),
    EXPLOSION_BLOCK(new BooleanFlag("explosions-blocks", false)),
    EXPLOSION_CREEPER_BLOCK(new BooleanFlag("creeper-explosion-entities", false)),
    EXPLOSION_CREEPER_ENTITY(new BooleanFlag("creeper-explosion-blocks", false)),
    EXPLOSION_OTHER_BLOCKS(new BooleanFlag("other-explosion-entities", false)),
    EXPLOSION_OTHER_ENTITY(new BooleanFlag("other-explosion-blocks", false)),
    IGNITE_EXPLOSIVES(new BooleanFlag("ignite-explosives", false)),
    TOOL_SECONDARY_USE(new BooleanFlag("tools-secondary", false)),
    AXE_STRIP(new BooleanFlag("strip-wood", false)),
    HOE_TILL(new BooleanFlag("till-farmland", false)),
    SHOVEL_PATH(new BooleanFlag("shovel-path", false)),
    TRAMPLE_FARMLAND(new BooleanFlag("trample-farmland", false)),
    TRAMPLE_FARMLAND_PLAYER(new BooleanFlag("trample-farmland-player", false)),
    TRAMPLE_FARMLAND_OTHER(new BooleanFlag("trample-farmland-other", false)),
    DRAGON_BLOCK_PROT(new BooleanFlag("dragon-destruction", false)),
    WITHER_BLOCK_PROT(new BooleanFlag("wither-destruction", false)),
    ZOMBIE_DOOR_PROT(new BooleanFlag("zombie-destruction", false)),
    LIGHTNING_PROT(new BooleanFlag("lightning", false)),
    //
    ANIMAL_TAMING(new BooleanFlag("animal-taming", false)),
    ANIMAL_BREEDING(new BooleanFlag("animal-breeding", false)),
    ANIMAL_MOUNTING(new BooleanFlag("animal-mounting", false)),
    ANIMAL_UNMOUNTING(new BooleanFlag("animal-unmounting", false)),
    SPAWNING_MONSTERS(new BooleanFlag("spawning-monsters", false)),
    SPAWNING_GOLEM(new BooleanFlag("spawning-irongolem", false)),
    SPAWNING_ANIMAL(new BooleanFlag("spawning-animal", false)),
    SPAWNING_ALL(new BooleanFlag("spawning-all", false)),
    SPAWNING_XP(new BooleanFlag("spawning-xp", false)),
    USE(new BooleanFlag("use", false)), // Buttons, Doors, Lever, e, false), falsetc
    USE_BONEMEAL(new BooleanFlag("use-bonemeal", false)),
    CONTAINER_ACCESS(new BooleanFlag("access-container", false)),
    ENDER_CHEST_ACCESS(new BooleanFlag("access-enderchest", false)),
    USE_ENDERPEARL_FROM_REGION(new BooleanFlag("enderpearl-from", false)),
    USE_ENDERPEARL_TO_REGION(new BooleanFlag("enderpearl-to", false)),
    ENDERMAN_TELEPORT_TO_REGION(new BooleanFlag("enderman-teleport-to", false)),
    ENDERMAN_TELEPORT_FROM_REGION(new BooleanFlag("enderman-teleport-from", false)),
    SHULKER_TELEPORT_TO_REGION(new BooleanFlag("shulker-teleport-to", false)),
    SHULKER_TELEPORT_FROM_REGION(new BooleanFlag("shulker-teleport-from", false)),
    ITEM_DROP(new BooleanFlag("item-drop", false)),
    // unrelated: mobs pickup logic => MobEntity#livingTick
    ITEM_PICKUP(new BooleanFlag("item-pickup", false)),
    LOOT_DROP(new BooleanFlag("loot-drop", false)),
    XP_DROP_ALL(new BooleanFlag("xp-drop-all", false)), // also includes blocks (furnace for example)
    XP_DROP_MONSTER(new BooleanFlag("xp-drop-monsters", false)), // only hostile mobs
    XP_DROP_OTHER(new BooleanFlag("xp-drop-other", false)), // non-hostile: animals, villagers,...
    XP_PICKUP(new BooleanFlag("xp-pickup", false)),
    LEVEL_FREEZE(new BooleanFlag("level-freeze", false)),
    XP_FREEZE(new BooleanFlag("xp-freeze", false)),
    ATTACK_PLAYERS(new BooleanFlag("attack-players", false)),
    ATTACK_ANIMALS(new BooleanFlag("attack-animals", false)),
    ATTACK_MONSTERS(new BooleanFlag("attack-monsters", false)),
    ATTACK_VILLAGERS(new BooleanFlag("attack-villagers", false)),
    ATTACK_WANDERING_TRADER(new BooleanFlag("attack-wtrader", false)),
    INVINCIBLE(new BooleanFlag("invincible", false)),
    FALL_DAMAGE(new BooleanFlag("fall-damage", false)),
    FALL_DAMAGE_VILLAGERS(new BooleanFlag("fall-damage-villagers", false)),
    FALL_DAMAGE_MONSTERS(new BooleanFlag("fall-damage-monsters", false)),
    FALL_DAMAGE_ANIMALS(new BooleanFlag("fall-damage-animals", false)),
    FALL_DAMAGE_PLAYERS(new BooleanFlag("fall-damage-players", false)),
    SEND_MESSAGE(new BooleanFlag("send-chat", false)),
    EXECUTE_COMMAND(new BooleanFlag("exec-command", false)),
    SET_SPAWN(new BooleanFlag("set-spawn", false)),
    // RESET_SPAWN(new BooleanFlag("reset-spawn"), // not working
    SLEEP(new BooleanFlag("sleep", false)),
    USE_PORTAL(new BooleanFlag("use-portal", false)),
    //TRAVEL_TO_DIM(new BooleanFlag("travel-to-dim", false)),
    //TRAVEL_FROM_DIM(new BooleanFlag("travel-from-dim", false)),
    USE_PORTAL_PLAYERS(new BooleanFlag("use-portal-players", false)),
    USE_PORTAL_ITEMS(new BooleanFlag("use-portal-items", false)),
    USE_PORTAL_ANIMALS(new BooleanFlag("use-portal-animals", false)),
    USE_PORTAL_MONSTERS(new BooleanFlag("use-portal-monsters", false)),
    USE_PORTAL_VILLAGERS(new BooleanFlag("use-portal-villagers", false)),
    USE_PORTAL_MINECARTS(new BooleanFlag("use-portal-minecarts", false)),
    SPAWN_PORTAL(new BooleanFlag("spawn-portal", false)),
    //PLAYER_SPAWN_PORTAL(new BooleanFlag("player-spawn-portal", false));

    SPAWN_ENTITLES(new ListFlag("spawn-entities", false));

    public final String flagname;
    public final IFlag flag;
    public final String langKey;
    // public final boolean defaultValue;
    public final Function<List<String>, TranslatableContents> denyMessageBuilder;

    RegionFlag(IFlag flag) {
        this.flag = flag;
        this.flagname = flag.getFlagIdentifier();
        this.langKey = "flag." + flag.getFlagIdentifier() + ".msg.deny";
        this.denyMessageBuilder = (args) -> new TranslatableContents(this.langKey, args == null ? new ArrayList<>() : args);
        //this.defaultValue = defaultValue;
    }
    @Override
    public String toString() {
        return flagname;
    }

    /**
     * Checks if a flagIdentifier is defined within the RegionFlag enum.
     * Replaces the check of FlagsList.VALID_FLAGS.contains(flag).
     * @param flagIdentifier to be checked
     * @return true if flagIdentifier is defined within this enum, false otherwise
     */
    public static boolean contains(String flagIdentifier) {
        return Arrays.stream(RegionFlag.values())
                .anyMatch(flag -> flag.toString().equals(flagIdentifier));
    }

    /**
     * Returns a set of all flags with their string representation defined within this enum.
     * @return a set of all flagIdentifiers defined within RegionFlag
     */
    public static List<String> getFlagNames() {
        return Arrays.stream(RegionFlag.values())
                .map(RegionFlag::toString)
                .collect(Collectors.toList());
    }

    public static Set<IFlag> getFlags() {
        return Arrays.stream(RegionFlag.values())
                .map(regionFlag -> regionFlag.flag)
                .collect(Collectors.toSet());
    }

    public static Set<IFlag> getFlags(FlagType type){
        return getFlags()
                .stream()
                .filter(flag -> flag.getFlagType() == type)
                .collect(Collectors.toSet());
    }

    public static Optional<RegionFlag> fromString(String flagIdentifier){
        return Arrays.stream(values())
                .filter(flag -> flag.flagname.equals(flagIdentifier))
                .findFirst();
    }

    public static Set<BooleanFlag> getBoolFlags(){
        return getFlags(FlagType.BOOLEAN_FLAG)
                .stream()
                .map(flag -> (BooleanFlag) flag)
                .collect(Collectors.toSet());
    }

    public static RegionFlag fromId(String flagIdentifier){
        return Arrays.stream(values())
                .filter(flag -> flag.flagname.equals(flagIdentifier))
                .collect(Collectors.toList()).get(0);
    }
}
