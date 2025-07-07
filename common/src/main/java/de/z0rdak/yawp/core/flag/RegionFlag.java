package de.z0rdak.yawp.core.flag;

import java.util.*;
import java.util.stream.Collectors;

public enum RegionFlag {

    ANIMAL_BREEDING("animal-breeding", FlagCategory.ENTITY, FlagCategory.PLAYER),
    ANIMAL_MOUNTING("animal-mounting", FlagCategory.ENTITY, FlagCategory.PLAYER),
    ANIMAL_TAMING("animal-taming", FlagCategory.ENTITY, FlagCategory.PLAYER),
    ANIMAL_UNMOUNTING("animal-unmounting", FlagCategory.ENTITY, FlagCategory.PLAYER),
    AXE_STRIP("strip-wood", FlagCategory.PLAYER, FlagCategory.BLOCK),
    BREAK_BLOCKS("break-blocks", FlagCategory.BLOCK, FlagCategory.PLAYER),
    CONTAINER_ACCESS("access-container", FlagCategory.BLOCK, FlagCategory.PLAYER),
    DRAGON_BLOCK_PROT("dragon-destruction", FlagCategory.ENVIRONMENT, FlagCategory.BLOCK),
    DROP_LOOT_ALL("drop-loot", FlagCategory.PLAYER_PREVENTION),
    DROP_LOOT_PLAYER("drop-loot-player", FlagCategory.PLAYER, FlagCategory.PLAYER_PREVENTION),
    ENDERMAN_GRIEFING("enderman-griefing", FlagCategory.PROTECTION),
    ENDERMAN_TELEPORT_FROM_REGION("enderman-tp-from", FlagCategory.PROTECTION),
    ENDER_CHEST_ACCESS("access-enderchest", FlagCategory.PLAYER, FlagCategory.BLOCK),
    ENTER_DIM("enter-dim", FlagCategory.PLAYER),
    EXECUTE_COMMAND("exec-command", FlagCategory.PLAYER),
    EXPLOSION_BLOCK("explosions-blocks", FlagCategory.PROTECTION),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-blocks", FlagCategory.PROTECTION),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-entities", FlagCategory.PROTECTION),
    EXPLOSION_ENTITY("explosions-entities", FlagCategory.PROTECTION),
    FALL_DAMAGE("fall-damage", FlagCategory.PROTECTION, FlagCategory.BENEFICIAL),
    FALL_DAMAGE_ANIMALS("fall-damage-animals", FlagCategory.PROTECTION),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters", FlagCategory.PROTECTION),
    FALL_DAMAGE_PLAYERS("fall-damage-players", FlagCategory.PROTECTION, FlagCategory.PLAYER),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers", FlagCategory.PROTECTION),
    FLUID_FLOW("fluid-flow", FlagCategory.ENVIRONMENT),
    HOE_TILL("till-farmland", FlagCategory.BLOCK, FlagCategory.PLAYER_PREVENTION),
    IGNITE_EXPLOSIVES("ignite-explosives", FlagCategory.BLOCK, FlagCategory.PLAYER_PREVENTION),
    INVINCIBLE("invincible", FlagCategory.BENEFICIAL),
    ITEM_DROP("item-drop", FlagCategory.PLAYER_PREVENTION),
    ITEM_PICKUP("item-pickup", FlagCategory.PLAYER_PREVENTION),
    KNOCKBACK_PLAYERS("knockback-players", FlagCategory.PLAYER, FlagCategory.BENEFICIAL, FlagCategory.ENTITY),
    LAVA_FLOW("lava-flow", FlagCategory.ENVIRONMENT),
    LEVEL_FREEZE("level-freeze", FlagCategory.PLAYER),
    LIGHTNING_PROT("lightning", FlagCategory.ENVIRONMENT),
    NO_WALKER_FREEZE("walker-freeze", FlagCategory.PLAYER, FlagCategory.BLOCK),
    LEAF_DECAY("leaf-decay", FlagCategory.ENVIRONMENT),
    FIRE_TICK("fire-tick", FlagCategory.ENVIRONMENT),
    MELEE_ANIMALS("melee-animals", FlagCategory.PLAYER, FlagCategory.ENTITY),
    MELEE_MONSTERS("melee-monsters", FlagCategory.PLAYER, FlagCategory.ENTITY),
    MELEE_PLAYERS("melee-players", FlagCategory.PLAYER, FlagCategory.ENTITY),
    MELEE_VILLAGERS("melee-villagers", FlagCategory.PLAYER, FlagCategory.ENTITY),
    MELEE_WANDERING_TRADER("melee-wtrader", FlagCategory.PLAYER, FlagCategory.ENTITY),
    MOB_GRIEFING("mob-griefing", FlagCategory.ENVIRONMENT),
    NO_FLIGHT("no-flight", FlagCategory.PLAYER),
    NO_ITEM_DESPAWN("no-item-despawn", FlagCategory.PROTECTION),
    NO_PVP("no-pvp", FlagCategory.PLAYER, FlagCategory.ENTITY),
    NO_SIGN_EDIT("no-sign-edit", FlagCategory.PLAYER, FlagCategory.BLOCK),
    PLACE_BLOCKS("place-blocks", FlagCategory.PLAYER, FlagCategory.BLOCK),
    PLACE_FLUIDS("place-fluids", FlagCategory.PLAYER, FlagCategory.BLOCK),
    SCOOP_FLUIDS("scoop-fluids", FlagCategory.PLAYER, FlagCategory.BLOCK),
    SEND_MESSAGE("send-chat", FlagCategory.PLAYER),
    SET_SPAWN("set-spawn", FlagCategory.PLAYER, FlagCategory.BLOCK),
    SHOVEL_PATH("shovel-path", FlagCategory.PLAYER, FlagCategory.BLOCK),
    SHULKER_TELEPORT_FROM_REGION("shulker-tp-from", FlagCategory.ENTITY),
    SLEEP("sleep", FlagCategory.PLAYER, FlagCategory.BLOCK),
    SNOW_FALL("snow-fall", FlagCategory.BLOCK, FlagCategory.ENVIRONMENT),
    SNOW_MELTING("snow-melting", FlagCategory.BLOCK, FlagCategory.ENVIRONMENT),
    SPAWNING_ALL("spawning-all", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_ANIMAL("spawning-animal", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_GOLEM("spawning-golem", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_MONSTER("spawning-monster", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_SLIME("spawning-slime", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_TRADER("spawning-trader", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_VILLAGER("spawning-villager", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWNING_XP("spawning-xp", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    SPAWN_PORTAL("spawn-portal", FlagCategory.PLAYER, FlagCategory.BLOCK),
    TOOL_SECONDARY_USE("tools-secondary", FlagCategory.PLAYER_PREVENTION),
    TRAMPLE_FARMLAND("trample-farmland", FlagCategory.ENVIRONMENT, FlagCategory.BLOCK),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other", FlagCategory.BLOCK, FlagCategory.ENTITY),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player", FlagCategory.PLAYER, FlagCategory.BLOCK),
    USE_BLOCKS("use-blocks", FlagCategory.PLAYER, FlagCategory.BLOCK),
    USE_BONEMEAL("use-bonemeal", FlagCategory.PLAYER, FlagCategory.BLOCK),
    USE_ELYTRA("use-elytra", FlagCategory.PLAYER),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from", FlagCategory.PLAYER, FlagCategory.BLOCK),
    USE_ENDERPEARL_TO_REGION("enderpearl-to", FlagCategory.PLAYER, FlagCategory.BLOCK),
    USE_ENTITIES("use-entities", FlagCategory.PLAYER, FlagCategory.ENTITY),
    USE_ITEMS("use-items", FlagCategory.PLAYER_PREVENTION),
    USE_PORTAL("use-portal", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    USE_PORTAL_ANIMALS("use-portal-animals", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    USE_PORTAL_ITEMS("use-portal-items", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    USE_PORTAL_MINECARTS("use-portal-minecarts", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    USE_PORTAL_MONSTERS("use-portal-monsters", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    USE_PORTAL_PLAYERS("use-portal-players", FlagCategory.PLAYER),
    USE_PORTAL_VILLAGERS("use-portal-villagers", FlagCategory.ENVIRONMENT, FlagCategory.ENTITY),
    WATER_FLOW("water-flow", FlagCategory.ENVIRONMENT),
    WITHER_BLOCK_PROT("wither-destruction", FlagCategory.ENVIRONMENT, FlagCategory.BLOCK),
    KEEP_XP("keep-xp", FlagCategory.BENEFICIAL, FlagCategory.PLAYER),
    //KEEP_INV("keep-inv", FlagCategory.BENEFICIAL, FlagCategory.PLAYER),
    //NO_HUNGER("no-hunger", FlagCategory.BENEFICIAL, FlagCategory.PLAYER),
    XP_DROP_ALL("xp-drop-all", FlagCategory.PLAYER_PREVENTION),
    XP_DROP_MONSTER("xp-drop-monsters", FlagCategory.PLAYER_PREVENTION),
    XP_DROP_OTHER("xp-drop-other", FlagCategory.PLAYER_PREVENTION),
    XP_DROP_PLAYER("xp-drop-player", FlagCategory.PLAYER, FlagCategory.PLAYER_PREVENTION),
    XP_FREEZE("xp-freeze", FlagCategory.PLAYER),
    XP_PICKUP("xp-pickup", FlagCategory.PLAYER),
    ZOMBIE_DOOR_PROT("zombie-destruction", FlagCategory.ENVIRONMENT, FlagCategory.PROTECTION);

    public final String name;
    public final FlagType type;
    public final List<FlagCategory> categories;

    RegionFlag(String name, FlagCategory...categories) {
        this.name = name;
        this.type = FlagType.BOOLEAN_FLAG;
        this.categories = Arrays.asList(categories);
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

    public static boolean hasPlayerCategory(IFlag flag) {
        Set<RegionFlag> flagsMatchingCategory = getFlagsMatchingCategory(FlagCategory.PLAYER);
        RegionFlag regionFlag = RegionFlag.fromId(flag.getName());
        return flagsMatchingCategory.contains(regionFlag);
    }

    public static boolean hasPlayerCategory(RegionFlag regionFlag) {
        return regionFlag.categories.contains(FlagCategory.PLAYER);
    }

    public static boolean hasCategory(RegionFlag regionFlag, FlagCategory category) {
        return regionFlag.categories.contains(category);
    }

    public static boolean matchesCategory(RegionFlag regionFlag, Set<String> categories) {
        Set<String> flagCategories = regionFlag.categories.stream().map(c -> c.name).collect(Collectors.toSet());
        return categories.stream().anyMatch(flagCategories::contains);
    }

    public static RegionFlag fromId(String flagIdentifier) throws IllegalArgumentException {
        List<RegionFlag> singleFlag = Arrays.stream(values())
                .filter(flag -> flag.name.equals(flagIdentifier))
                .toList();
        if (singleFlag.isEmpty()) {
            throw new IllegalArgumentException("Invalid region flag identifier supplied");
        }
        return singleFlag.get(0);
    }

    @Override
    public String toString() {
        return name;
    }

    public boolean isBeneficial() {
        return this.categories.contains(FlagCategory.BENEFICIAL);
    }
}