package de.z0rdak.yawp.core.flag;

import java.util.*;
import java.util.stream.Collectors;

public enum RegionFlag {

    ANIMAL_BREEDING("animal-breeding", FlagTag.ENTITY, FlagTag.PLAYER),
    ANIMAL_MOUNTING("animal-mounting", FlagTag.ENTITY, FlagTag.PLAYER),
    ANIMAL_TAMING("animal-taming", FlagTag.ENTITY, FlagTag.PLAYER),
    ANIMAL_UNMOUNTING("animal-unmounting", FlagTag.ENTITY, FlagTag.PLAYER),
    AXE_STRIP("strip-wood", FlagTag.PLAYER, FlagTag.BLOCK),
    BREAK_BLOCKS("break-blocks", FlagTag.BLOCK, FlagTag.PLAYER),
    CONTAINER_ACCESS("access-container", FlagTag.BLOCK, FlagTag.PLAYER),
    DRAGON_BLOCK_PROT("dragon-destruction", FlagTag.ENVIRONMENT, FlagTag.BLOCK),
    DROP_LOOT_ALL("drop-loot", FlagTag.PLAYER_PREVENTION),
    DROP_LOOT_PLAYER("drop-loot-player", FlagTag.PLAYER, FlagTag.PLAYER_PREVENTION),
    ENDERMAN_GRIEFING("enderman-griefing", FlagTag.PROTECTION),
    ENDERMAN_TELEPORT_FROM_REGION("enderman-tp-from", FlagTag.PROTECTION),
    ENDER_CHEST_ACCESS("access-enderchest", FlagTag.PLAYER, FlagTag.BLOCK),
    ENTER_DIM("enter-dim", FlagTag.PLAYER),
    EXECUTE_COMMAND("exec-command", FlagTag.PLAYER),
    EXPLOSION_BLOCK("explosions-blocks", FlagTag.PROTECTION),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-blocks", FlagTag.PROTECTION),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-entities", FlagTag.PROTECTION),
    EXPLOSION_ENTITY("explosions-entities", FlagTag.PROTECTION),
    FALL_DAMAGE("fall-damage", FlagTag.PROTECTION, FlagTag.BENEFICIAL),
    FALL_DAMAGE_ANIMALS("fall-damage-animals", FlagTag.PROTECTION),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters", FlagTag.PROTECTION),
    FALL_DAMAGE_PLAYERS("fall-damage-players", FlagTag.PROTECTION, FlagTag.PLAYER),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers", FlagTag.PROTECTION),
    FLUID_FLOW("fluid-flow", FlagTag.ENVIRONMENT),
    HOE_TILL("till-farmland", FlagTag.BLOCK, FlagTag.PLAYER_PREVENTION),
    IGNITE_EXPLOSIVES("ignite-explosives", FlagTag.BLOCK, FlagTag.PLAYER_PREVENTION),
    INVINCIBLE("invincible", FlagTag.BENEFICIAL),
    ITEM_DROP("item-drop", FlagTag.PLAYER_PREVENTION),
    ITEM_PICKUP("item-pickup", FlagTag.PLAYER_PREVENTION),
    KNOCKBACK_PLAYERS("knockback-players", FlagTag.PLAYER, FlagTag.BENEFICIAL, FlagTag.ENTITY),
    LAVA_FLOW("lava-flow", FlagTag.ENVIRONMENT),
    LEVEL_FREEZE("level-freeze", FlagTag.PLAYER),
    LIGHTNING_PROT("lightning", FlagTag.ENVIRONMENT),
    NO_WALKER_FREEZE("walker-freeze", FlagTag.PLAYER, FlagTag.BLOCK),
    LEAF_DECAY("leaf-decay", FlagTag.ENVIRONMENT),
    FIRE_TICK("fire-tick", FlagTag.ENVIRONMENT),
    MELEE_ANIMALS("melee-animals", FlagTag.PLAYER, FlagTag.ENTITY),
    MELEE_MONSTERS("melee-monsters", FlagTag.PLAYER, FlagTag.ENTITY),
    MELEE_PLAYERS("melee-players", FlagTag.PLAYER, FlagTag.ENTITY),
    MELEE_VILLAGERS("melee-villagers", FlagTag.PLAYER, FlagTag.ENTITY),
    MELEE_WANDERING_TRADER("melee-wtrader", FlagTag.PLAYER, FlagTag.ENTITY),
    MOB_GRIEFING("mob-griefing", FlagTag.ENVIRONMENT),
    NO_FLIGHT("no-flight", FlagTag.PLAYER),
    NO_ITEM_DESPAWN("no-item-despawn", FlagTag.PROTECTION),
    NO_PVP("no-pvp", FlagTag.PLAYER, FlagTag.ENTITY),
    FIRE_BOW("fire-bow", FlagTag.PLAYER, FlagTag.ENTITY),
    NO_SIGN_EDIT("no-sign-edit", FlagTag.PLAYER, FlagTag.BLOCK),
    PLACE_BLOCKS("place-blocks", FlagTag.PLAYER, FlagTag.BLOCK),
    PLACE_FLUIDS("place-fluids", FlagTag.PLAYER, FlagTag.BLOCK),
    SCOOP_FLUIDS("scoop-fluids", FlagTag.PLAYER, FlagTag.BLOCK),
    SEND_MESSAGE("send-chat", FlagTag.PLAYER),
    SET_SPAWN("set-spawn", FlagTag.PLAYER, FlagTag.BLOCK),
    SHOVEL_PATH("shovel-path", FlagTag.PLAYER, FlagTag.BLOCK),
    SHULKER_TELEPORT_FROM_REGION("shulker-tp-from", FlagTag.ENTITY),
    SLEEP("sleep", FlagTag.PLAYER, FlagTag.BLOCK),
    SNOW_FALL("snow-fall", FlagTag.BLOCK, FlagTag.ENVIRONMENT),
    SNOW_MELTING("snow-melting", FlagTag.BLOCK, FlagTag.ENVIRONMENT),
    SPAWNING_ALL("spawning-all", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_ANIMAL("spawning-animal", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_GOLEM("spawning-golem", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_MONSTER("spawning-monster", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_SLIME("spawning-slime", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_TRADER("spawning-trader", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_VILLAGER("spawning-villager", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWNING_XP("spawning-xp", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    SPAWN_PORTAL("spawn-portal", FlagTag.PLAYER, FlagTag.BLOCK),
    TOOL_SECONDARY_USE("tools-secondary", FlagTag.PLAYER_PREVENTION),
    TRAMPLE_FARMLAND("trample-farmland", FlagTag.ENVIRONMENT, FlagTag.BLOCK),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other", FlagTag.BLOCK, FlagTag.ENTITY),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player", FlagTag.PLAYER, FlagTag.BLOCK),
    USE_BLOCKS("use-blocks", FlagTag.PLAYER, FlagTag.BLOCK),
    USE_BONEMEAL("use-bonemeal", FlagTag.PLAYER, FlagTag.BLOCK),
    USE_ELYTRA("use-elytra", FlagTag.PLAYER),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from", FlagTag.PLAYER, FlagTag.BLOCK),
    USE_ENDERPEARL_TO_REGION("enderpearl-to", FlagTag.PLAYER, FlagTag.BLOCK),
    USE_ENTITIES("use-entities", FlagTag.PLAYER, FlagTag.ENTITY),
    USE_ITEMS("use-items", FlagTag.PLAYER_PREVENTION),
    USE_PORTAL("use-portal", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    USE_PORTAL_ANIMALS("use-portal-animals", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    USE_PORTAL_ITEMS("use-portal-items", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    USE_PORTAL_MINECARTS("use-portal-minecarts", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    USE_PORTAL_MONSTERS("use-portal-monsters", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    USE_PORTAL_PLAYERS("use-portal-players", FlagTag.PLAYER),
    USE_PORTAL_VILLAGERS("use-portal-villagers", FlagTag.ENVIRONMENT, FlagTag.ENTITY),
    WATER_FLOW("water-flow", FlagTag.ENVIRONMENT),
    WITHER_BLOCK_PROT("wither-destruction", FlagTag.ENVIRONMENT, FlagTag.BLOCK),
    KEEP_XP("keep-xp", FlagTag.BENEFICIAL, FlagTag.PLAYER),
    //KEEP_INV("keep-inv", FlagCategory.BENEFICIAL, FlagCategory.PLAYER),
    //NO_HUNGER("no-hunger", FlagCategory.BENEFICIAL, FlagCategory.PLAYER),
    XP_DROP_ALL("xp-drop-all", FlagTag.PLAYER_PREVENTION),
    XP_DROP_MONSTER("xp-drop-monsters", FlagTag.PLAYER_PREVENTION),
    XP_DROP_OTHER("xp-drop-other", FlagTag.PLAYER_PREVENTION),
    XP_DROP_PLAYER("xp-drop-player", FlagTag.PLAYER, FlagTag.PLAYER_PREVENTION),
    XP_FREEZE("xp-freeze", FlagTag.PLAYER),
    XP_PICKUP("xp-pickup", FlagTag.PLAYER),
    ZOMBIE_DOOR_PROT("zombie-destruction", FlagTag.ENVIRONMENT, FlagTag.PROTECTION);

    public final String name;
    public final FlagType type;
    public final List<FlagTag> categories;

    RegionFlag(String name, FlagTag...categories) {
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

    public static Set<RegionFlag> getFlagsMatchingCategory(FlagTag category) {
        return getFlags().stream()
                .filter(flag -> flag.categories.contains(category))
                .collect(Collectors.toSet());
    }

    public static boolean hasPlayerCategory(IFlag flag) {
        Set<RegionFlag> flagsMatchingCategory = getFlagsMatchingCategory(FlagTag.PLAYER);
        RegionFlag regionFlag = RegionFlag.fromId(flag.getName());
        return flagsMatchingCategory.contains(regionFlag);
    }

    public static boolean hasPlayerCategory(RegionFlag regionFlag) {
        return regionFlag.categories.contains(FlagTag.PLAYER);
    }

    public static boolean hasCategory(RegionFlag regionFlag, FlagTag category) {
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
        return this.categories.contains(FlagTag.BENEFICIAL);
    }
}