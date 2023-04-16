package de.z0rdak.yawp.core.flag;

import java.util.*;
import java.util.stream.Collectors;

public enum RegionFlag {

    ANIMAL_BREEDING("animal-breeding", FlagType.BOOLEAN_FLAG),
    ANIMAL_MOUNTING("animal-mounting", FlagType.BOOLEAN_FLAG),
    ANIMAL_TAMING("animal-taming", FlagType.BOOLEAN_FLAG),
    ANIMAL_UNMOUNTING("animal-unmounting", FlagType.BOOLEAN_FLAG),
    AXE_STRIP("strip-wood", FlagType.BOOLEAN_FLAG),
    BREAK_BLOCKS("break-blocks", FlagType.BOOLEAN_FLAG),
    CONTAINER_ACCESS("access-container", FlagType.BOOLEAN_FLAG),
    DRAGON_BLOCK_PROT("dragon-destruction", FlagType.BOOLEAN_FLAG),
    DROP_LOOT_ALL("drop-loot", FlagType.BOOLEAN_FLAG),
    DROP_LOOT_PLAYER("drop-loot-player", FlagType.BOOLEAN_FLAG),
    // ENDERMAN_GRIEFING("enderman-griefing", FlagType.BOOLEAN_FLAG), // FIXME: soon
    ENDERMAN_TELEPORT_FROM_REGION("enderman-tp-from", FlagType.BOOLEAN_FLAG),
    ENDER_CHEST_ACCESS("access-enderchest", FlagType.BOOLEAN_FLAG),
    ENTER_DIM("enter-dim", FlagType.BOOLEAN_FLAG),
    EXECUTE_COMMAND("exec-command", FlagType.BOOLEAN_FLAG),
    EXPLOSION_BLOCK("explosions-blocks", FlagType.BOOLEAN_FLAG),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-entities", FlagType.BOOLEAN_FLAG),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-blocks", FlagType.BOOLEAN_FLAG),
    EXPLOSION_ENTITY("explosions-entities", FlagType.BOOLEAN_FLAG),
    EXPLOSION_OTHER_BLOCKS("other-explosion-entities", FlagType.BOOLEAN_FLAG),
    EXPLOSION_OTHER_ENTITY("other-explosion-blocks", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE("fall-damage", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_ANIMALS("fall-damage-animals", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_PLAYERS("fall-damage-players", FlagType.BOOLEAN_FLAG),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers", FlagType.BOOLEAN_FLAG),
    HOE_TILL("till-farmland", FlagType.BOOLEAN_FLAG),
    IGNITE_EXPLOSIVES("ignite-explosives", FlagType.BOOLEAN_FLAG),
    INVINCIBLE("invincible", FlagType.BOOLEAN_FLAG),
    ITEM_DROP("item-drop", FlagType.BOOLEAN_FLAG),
    ITEM_PICKUP("item-pickup", FlagType.BOOLEAN_FLAG),
    KNOCKBACK_PLAYERS("knockback-players", FlagType.BOOLEAN_FLAG),
    LEVEL_FREEZE("level-freeze", FlagType.BOOLEAN_FLAG),
    LIGHTNING_PROT("lightning", FlagType.BOOLEAN_FLAG),
    //NO_MELTING("melting", FlagType.BOOLEAN_FLAG),
    //NO_WATER_FREEZE("water-freeze", FlagType.BOOLEAN_FLAG),
    NO_WALKER_FREEZE("walker-freeze", FlagType.BOOLEAN_FLAG),
    LEAF_DECAY("leaf-decay", FlagType.BOOLEAN_FLAG),
    FIRE_TICK("fire-tick", FlagType.BOOLEAN_FLAG),
    MELEE_ANIMALS("melee-animals", FlagType.BOOLEAN_FLAG),
    MELEE_MONSTERS("melee-monsters", FlagType.BOOLEAN_FLAG),
    MELEE_PLAYERS("melee-players", FlagType.BOOLEAN_FLAG),
    MELEE_VILLAGERS("melee-villagers", FlagType.BOOLEAN_FLAG),
    MELEE_WANDERING_TRADER("melee-wtrader", FlagType.BOOLEAN_FLAG),
    // MOB_GRIEFING("mob-griefing", FlagType.BOOLEAN_FLAG), // FIXME: soon
    NO_FLIGHT("no-flight", FlagType.BOOLEAN_FLAG),
    NO_PVP("no-pvp", FlagType.BOOLEAN_FLAG),
    PLACE_BLOCKS("place-blocks", FlagType.BOOLEAN_FLAG),
    // PLACE_FLUIDS("place-fluids", FlagType.BOOLEAN_FLAG), // TODO:
    // SCOOP_FLUIDS("scoop-fluids", FlagType.BOOLEAN_FLAG), // TODO:
    // SEND_MESSAGE("send-chat", FlagType.BOOLEAN_FLAG), // FIXME: soon
    SET_SPAWN("set-spawn", FlagType.BOOLEAN_FLAG),
    SHOVEL_PATH("shovel-path", FlagType.BOOLEAN_FLAG),
    SHULKER_TELEPORT_FROM_REGION("shulker-tp-from", FlagType.BOOLEAN_FLAG),
    SLEEP("sleep", FlagType.BOOLEAN_FLAG),
    SPAWNING_ALL("spawning-all", FlagType.BOOLEAN_FLAG),
    SPAWNING_ANIMAL("spawning-animal", FlagType.BOOLEAN_FLAG),
    SPAWNING_GOLEM("spawning-golem", FlagType.BOOLEAN_FLAG),
    SPAWNING_MONSTER("spawning-monster", FlagType.BOOLEAN_FLAG),
    SPAWNING_SLIME("spawning-slime", FlagType.BOOLEAN_FLAG),
    SPAWNING_TRADER("spawning-trader", FlagType.BOOLEAN_FLAG),
    SPAWNING_VILLAGER("spawning-villager", FlagType.BOOLEAN_FLAG),
    SPAWNING_XP("spawning-xp", FlagType.BOOLEAN_FLAG),
    SPAWN_PORTAL("spawn-portal", FlagType.BOOLEAN_FLAG),
    TOOL_SECONDARY_USE("tools-secondary", FlagType.BOOLEAN_FLAG),
    TRAMPLE_FARMLAND("trample-farmland", FlagType.BOOLEAN_FLAG),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other", FlagType.BOOLEAN_FLAG),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player", FlagType.BOOLEAN_FLAG),
    USE_BLOCKS("use-blocks", FlagType.BOOLEAN_FLAG),
    USE_BONEMEAL("use-bonemeal", FlagType.BOOLEAN_FLAG),
    USE_ELYTRA("use-elytra", FlagType.BOOLEAN_FLAG),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from", FlagType.BOOLEAN_FLAG),
    USE_ENDERPEARL_TO_REGION("enderpearl-to", FlagType.BOOLEAN_FLAG),
    USE_ENTITIES("use-entities", FlagType.BOOLEAN_FLAG),
    USE_ITEMS("use-items", FlagType.BOOLEAN_FLAG),
    USE_PORTAL("use-portal", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_ANIMALS("use-portal-animals", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_ITEMS("use-portal-items", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_MINECARTS("use-portal-minecarts", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_MONSTERS("use-portal-monsters", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_PLAYERS("use-portal-players", FlagType.BOOLEAN_FLAG),
    USE_PORTAL_VILLAGERS("use-portal-villagers", FlagType.BOOLEAN_FLAG),
    WITHER_BLOCK_PROT("wither-destruction", FlagType.BOOLEAN_FLAG),
    XP_DROP_ALL("xp-drop-all", FlagType.BOOLEAN_FLAG),
    XP_DROP_MONSTER("xp-drop-monsters", FlagType.BOOLEAN_FLAG),
    XP_DROP_OTHER("xp-drop-other", FlagType.BOOLEAN_FLAG),
    XP_DROP_PLAYER("xp-drop-player", FlagType.BOOLEAN_FLAG),
    XP_FREEZE("xp-freeze", FlagType.BOOLEAN_FLAG),
    XP_PICKUP("xp-pickup", FlagType.BOOLEAN_FLAG);
    // ZOMBIE_DOOR_PROT("zombie-destruction", FlagType.BOOLEAN_FLAG); // FIXME: soon

    public final String name;
    public final FlagType type;

    RegionFlag(String name, FlagType type) {
        this.name = name;
        this.type = type;
    }

    /**
     * Checks if a flagIdentifier is defined within the RegionFlag enum.
     * Replaces the check of FlagsList.VALID_FLAGS.contains(flag).
     *
     * @param flagIdentifier to be checked
     * @return true if flagIdentifier is defined within this enum, false otherwise
     */
    public static boolean contains(String flagIdentifier) {
        return Arrays.stream(RegionFlag.values())
                .anyMatch(flag -> flag.toString().equals(flagIdentifier));
    }

    /**
     * Returns a set of all flags with their string representation defined within this enum.
     *
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

    public static Set<RegionFlag> getFlags(FlagType type) {
        return getFlags()
                .stream()
                .filter(flag -> flag.type.equals(type))
                .collect(Collectors.toSet());
    }

    public static Optional<RegionFlag> fromString(String flagIdentifier) {
        return Arrays.stream(values())
                .filter(flag -> flag.name.equals(flagIdentifier))
                .findFirst();
    }

    public static Set<RegionFlag> getBoolFlags() {
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

    @Override
    public String toString() {
        return name;
    }
}
