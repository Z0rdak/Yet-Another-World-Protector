package de.z0rdak.yawp.core.flag;

import java.util.*;
import java.util.stream.Collectors;

public enum RegionFlag {

    BREAK_BLOCKS("break_blocks", FlagType.BOOLEAN_FLAG),
    SCOOP_FLUIDS("scoop_fluids", FlagType.BOOLEAN_FLAG),
    BREAK_ENTITIES("break_entities", FlagType.BOOLEAN_FLAG),
    PLACE_BLOCKS("place_blocks", FlagType.BOOLEAN_FLAG),
    PLACE_FLUIDS("place_fluids", FlagType.BOOLEAN_FLAG),
    ENTITY_PLACE("entity-place", FlagType.BOOLEAN_FLAG),
    EXPLOSION_ENTITY("explosions-entities", FlagType.BOOLEAN_FLAG),
    EXPLOSION_BLOCK("explosions-blocks", FlagType.BOOLEAN_FLAG),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-entities", FlagType.BOOLEAN_FLAG),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-blocks", FlagType.BOOLEAN_FLAG),
    EXPLOSION_OTHER_BLOCKS("other-explosion-entities", FlagType.BOOLEAN_FLAG),
    EXPLOSION_OTHER_ENTITY("other-explosion-blocks", FlagType.BOOLEAN_FLAG),
    IGNITE_EXPLOSIVES("ignite-explosives", FlagType.BOOLEAN_FLAG),
    TOOL_SECONDARY_USE("tools-secondary", FlagType.BOOLEAN_FLAG),
    AXE_STRIP("strip-wood", FlagType.BOOLEAN_FLAG),
    HOE_TILL("till-farmland", FlagType.BOOLEAN_FLAG),
    SHOVEL_PATH("shovel-path", FlagType.BOOLEAN_FLAG),
    TRAMPLE_FARMLAND("trample-farmland", FlagType.BOOLEAN_FLAG),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player", FlagType.BOOLEAN_FLAG),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other", FlagType.BOOLEAN_FLAG),
    DRAGON_BLOCK_PROT("dragon-destruction", FlagType.BOOLEAN_FLAG),
    WITHER_BLOCK_PROT("wither-destruction", FlagType.BOOLEAN_FLAG),
    ZOMBIE_DOOR_PROT("zombie-destruction", FlagType.BOOLEAN_FLAG),
    LIGHTNING_PROT("lightning", FlagType.BOOLEAN_FLAG),
    //
    ANIMAL_TAMING("animal-taming", FlagType.BOOLEAN_FLAG),
    ANIMAL_BREEDING("animal-breeding", FlagType.BOOLEAN_FLAG),
    ANIMAL_MOUNTING("animal-mounting", FlagType.BOOLEAN_FLAG),
    // ANIMAL_UNMOUNTING("animal-unmounting", FlagType.BOOLEAN_FLAG)), // FIXME: Minecraft vanilla bug fixed in 1.17 snapshot - mention in wiki
    SPAWNING_MONSTERS("spawning-monsters", FlagType.BOOLEAN_FLAG),
    SPAWNING_GOLEM("spawning-irongolem", FlagType.BOOLEAN_FLAG),
    SPAWNING_ANIMAL("spawning-animal", FlagType.BOOLEAN_FLAG),
    SPAWNING_ALL("spawning-all", FlagType.BOOLEAN_FLAG),
    SPAWNING_XP("spawning-xp", FlagType.BOOLEAN_FLAG),
    USE("use", FlagType.BOOLEAN_FLAG),
    USE_BONEMEAL("use-bonemeal", FlagType.BOOLEAN_FLAG),
    CONTAINER_ACCESS("access-container", FlagType.BOOLEAN_FLAG),
    ENDER_CHEST_ACCESS("access-enderchest", FlagType.BOOLEAN_FLAG),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from", FlagType.BOOLEAN_FLAG),
    USE_ENDERPEARL_TO_REGION("enderpearl-to", FlagType.BOOLEAN_FLAG),
    ENDERMAN_TELEPORT_TO_REGION("enderman-teleport-to", FlagType.BOOLEAN_FLAG),
    ENDERMAN_TELEPORT_FROM_REGION("enderman-teleport-from", FlagType.BOOLEAN_FLAG),
    SHULKER_TELEPORT_TO_REGION("shulker-teleport-to", FlagType.BOOLEAN_FLAG),
    SHULKER_TELEPORT_FROM_REGION("shulker-teleport-from", FlagType.BOOLEAN_FLAG),
    ITEM_DROP("item-drop", FlagType.BOOLEAN_FLAG),
    // unrelated: mobs pickup logic => MobEntity#livingTick
    ITEM_PICKUP("item-pickup", FlagType.BOOLEAN_FLAG),
    LOOT_DROP("loot-drop", FlagType.BOOLEAN_FLAG),
    XP_DROP_ALL("xp-drop-all", FlagType.BOOLEAN_FLAG), // also includes blocks (furnace for FlagType.BOOLEAN_FLAGl)
    XP_DROP_MONSTER("xp-drop-monsters", FlagType.BOOLEAN_FLAG), // only hostFlagType.BOOLEAN_FLAGos
    XP_DROP_OTHER("xp-drop-other", FlagType.BOOLEAN_FLAG), // non-hostile: animals, villaFlagType.BOOLEAN_FLAG..
    XP_PICKUP("xp-pickup", FlagType.BOOLEAN_FLAG),
    LEVEL_FREEZE("level-freeze", FlagType.BOOLEAN_FLAG),
    XP_FREEZE("xp-freeze", FlagType.BOOLEAN_FLAG),
    ATTACK_PLAYERS("attack-players", FlagType.BOOLEAN_FLAG),
    ATTACK_ANIMALS("attack-animals", FlagType.BOOLEAN_FLAG),
    ATTACK_MONSTERS("attack-monsters", FlagType.BOOLEAN_FLAG),
    ATTACK_VILLAGERS("attack-villagers", FlagType.BOOLEAN_FLAG),
    ATTACK_WANDERING_TRADER("attack-wtrader", FlagType.BOOLEAN_FLAG),
    INVINCIBLE("invincible", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE("fall-damage", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_ANIMALS("fall-damage-animals", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_PLAYERS("fall-damage-players", FlagType.BOOLEAN_FLAG),
    SEND_MESSAGE("send-chat", FlagType.BOOLEAN_FLAG),
    EXECUTE_COMMAND("exec-command", FlagType.BOOLEAN_FLAG),
    SET_SPAWN("set-spawn", FlagType.BOOLEAN_FLAG),
    SLEEP("sleep", FlagType.BOOLEAN_FLAG),
    USE_PORTAL("use-portal", FlagType.BOOLEAN_FLAG),
    //TRAVEL_TO_DIM("travel-to-dim", FlagType.BOOLEAN_FLAG),
    //TRAVEL_FROM_DIM("travel-from-dim", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_PLAYERS("use-portal-players", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_ITEMS("use-portal-items", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_ANIMALS("use-portal-animals", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_MONSTERS("use-portal-monsters", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_VILLAGERS("use-portal-villagers", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_MINECARTS("use-portal-minecarts", FlagType.BOOLEAN_FLAG),
    SPAWN_PORTAL("spawn-portal", FlagType.BOOLEAN_FLAG),
    //PLAYER_SPAWN_PORTAL("player-spawn-portal", FlagType.BOOLEAN_FLAG);

    SPAWN_ENTITLES("spawn-entities", FlagType.LIST_FLAG);

    public final String name;
    public final FlagType type;

    RegionFlag(String name, FlagType type) {
        this.name = name;
        this.type = type;
    }
    @Override
    public String toString() {
        return name;
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

    public static Set<RegionFlag> getFlags() {
        return Arrays.stream(RegionFlag.values())
                .collect(Collectors.toSet());
    }


    public static Set<RegionFlag> getFlags(FlagType type){
        return getFlags()
                .stream()
                .filter(flag -> flag.type.equals(type))
                .collect(Collectors.toSet());
    }

    public static Optional<RegionFlag> fromString(String flagIdentifier){
        return Arrays.stream(values())
                .filter(flag -> flag.name.equals(flagIdentifier))
                .findFirst();
    }

    public static Set<RegionFlag> getBoolFlags(){
        return new HashSet<>(getFlags(FlagType.BOOLEAN_FLAG));
    }

    public static RegionFlag fromId(String flagIdentifier) throws IllegalArgumentException {
        List<RegionFlag> singleFlag = Arrays.stream(values())
                .filter(flag -> flag.name.equals(flagIdentifier))
                .collect(Collectors.toList());
        if (singleFlag.size() == 0) {
            throw new IllegalArgumentException("Invalid region flag identifier supplied");
        }
        return singleFlag.get(0);
    }
}
