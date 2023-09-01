package de.z0rdak.yawp.core.flag;

import java.util.*;
import java.util.stream.Collectors;

public enum RegionFlag {

    ANIMAL_BREEDING("animal-breeding", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENTITY, FlagCategory.PLAYER)),
    ANIMAL_MOUNTING("animal-mounting", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENTITY, FlagCategory.PLAYER)),
    ANIMAL_TAMING("animal-taming", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENTITY, FlagCategory.PLAYER)),
    ANIMAL_UNMOUNTING("animal-unmounting", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENTITY, FlagCategory.PLAYER)),
    AXE_STRIP("strip-wood", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    BREAK_BLOCKS("break-blocks", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.BLOCK, FlagCategory.PLAYER)),
    CONTAINER_ACCESS("access-container", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.BLOCK, FlagCategory.PLAYER)),
    DRAGON_BLOCK_PROT("dragon-destruction", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.BLOCK)),
    DROP_LOOT_ALL("drop-loot", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    DROP_LOOT_PLAYER("drop-loot-player", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    ENDERMAN_GRIEFING("enderman-griefing", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    ENDERMAN_TELEPORT_FROM_REGION("enderman-tp-from", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    ENDER_CHEST_ACCESS("access-enderchest", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    ENTER_DIM("enter-dim", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    EXECUTE_COMMAND("exec-command", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    EXPLOSION_BLOCK("explosions-blocks", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-entities", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-blocks", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    EXPLOSION_ENTITY("explosions-entities", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    EXPLOSION_OTHER_BLOCKS("other-explosion-entities", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    EXPLOSION_OTHER_ENTITY("other-explosion-blocks", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    FALL_DAMAGE("fall-damage", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PROTECTION)),
    FALL_DAMAGE_ANIMALS("fall-damage-animals", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PROTECTION)),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PROTECTION)),
    FALL_DAMAGE_PLAYERS("fall-damage-players", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PROTECTION)),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PROTECTION)),
    HOE_TILL("till-farmland", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.BLOCK, FlagCategory.PLAYER)),
    IGNITE_EXPLOSIVES("ignite-explosives", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.BLOCK, FlagCategory.PLAYER)),
    INVINCIBLE("invincible", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PROTECTION)),
    ITEM_DROP("item-drop", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    ITEM_PICKUP("item-pickup", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    KNOCKBACK_PLAYERS("knockback-players", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    LEVEL_FREEZE("level-freeze", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    LIGHTNING_PROT("lightning", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.ENVIRONMENT)),
    //NO_MELTING("melting", FlagType.BOOLEAN_FLAG, Arrays.asList()),
    //NO_WATER_FREEZE("water-freeze", FlagType.BOOLEAN_FLAG, Arrays.asList()),
    NO_WALKER_FREEZE("walker-freeze", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    LEAF_DECAY("leaf-decay", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.ENVIRONMENT)),
    FIRE_TICK("fire-tick", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.ENVIRONMENT)),
    MELEE_ANIMALS("melee-animals", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    MELEE_MONSTERS("melee-monsters", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    MELEE_PLAYERS("melee-players", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    MELEE_VILLAGERS("melee-villagers", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    MELEE_WANDERING_TRADER("melee-wtrader", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    MOB_GRIEFING("mob-griefing", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.ENVIRONMENT)),
    NO_FLIGHT("no-flight", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    NO_PVP("no-pvp", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    PLACE_BLOCKS("place-blocks", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    PLACE_FLUIDS("place-fluids", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    SCOOP_FLUIDS("scoop-fluids", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    SEND_MESSAGE("send-chat", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    SET_SPAWN("set-spawn", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    SHOVEL_PATH("shovel-path", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    SHULKER_TELEPORT_FROM_REGION("shulker-tp-from", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.ENTITY)),
    SLEEP("sleep", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    SPAWNING_ALL("spawning-all", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_ANIMAL("spawning-animal", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_GOLEM("spawning-golem", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_MONSTER("spawning-monster", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_SLIME("spawning-slime", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_TRADER("spawning-trader", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_VILLAGER("spawning-villager", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWNING_XP("spawning-xp", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    SPAWN_PORTAL("spawn-portal", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    TOOL_SECONDARY_USE("tools-secondary", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ITEM)),
    TRAMPLE_FARMLAND("trample-farmland", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.BLOCK)),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.BLOCK, FlagCategory.ENTITY)),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    USE_BLOCKS("use-blocks", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    USE_BONEMEAL("use-bonemeal", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    USE_ELYTRA("use-elytra", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    USE_ENDERPEARL_TO_REGION("enderpearl-to", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.BLOCK)),
    USE_ENTITIES("use-entities", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ENTITY)),
    USE_ITEMS("use-items", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.PLAYER, FlagCategory.ITEM)),
    USE_PORTAL("use-portal", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    USE_PORTAL_ANIMALS("use-portal-animals", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    USE_PORTAL_ITEMS("use-portal-items", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    USE_PORTAL_MINECARTS("use-portal-minecarts", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    USE_PORTAL_MONSTERS("use-portal-monsters", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    USE_PORTAL_PLAYERS("use-portal-players", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    USE_PORTAL_VILLAGERS("use-portal-villagers", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY)),
    WITHER_BLOCK_PROT("wither-destruction", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.BLOCK)),
    XP_DROP_ALL("xp-drop-all", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    XP_DROP_MONSTER("xp-drop-monsters", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    XP_DROP_OTHER("xp-drop-other", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    XP_DROP_PLAYER("xp-drop-player", FlagType.BOOLEAN_FLAG, Collections.emptyList()),
    XP_FREEZE("xp-freeze", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    XP_PICKUP("xp-pickup", FlagType.BOOLEAN_FLAG, Collections.singletonList(FlagCategory.PLAYER)),
    ZOMBIE_DOOR_PROT("zombie-destruction", FlagType.BOOLEAN_FLAG, Arrays.asList(FlagCategory.ENVIRONMENT, FlagCategory.ENTITY));

    public final String name;
    public final FlagType type;
    public final List<FlagCategory> categories;

    RegionFlag(String name, FlagType type, List<FlagCategory> categories) {
        this.name = name;
        this.type = type;
        this.categories = categories;
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

    public static Set<RegionFlag> getFlagsMatchingCategory(FlagCategory category) {
        return getFlags().stream()
                .filter(flag -> flag.categories.contains(category))
                .collect(Collectors.toSet());
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
