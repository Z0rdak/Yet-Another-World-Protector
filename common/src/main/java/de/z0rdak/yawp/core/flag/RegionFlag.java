package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.api.FlagTagRegister;

import java.util.*;
import java.util.stream.Collectors;

public enum RegionFlag {

    ANIMAL_BREEDING("animal-breeding", FlagTagRegister.ENTITY, FlagTagRegister.PLAYER),
    ANIMAL_MOUNTING("animal-mounting", FlagTagRegister.ENTITY, FlagTagRegister.PLAYER),
    ANIMAL_TAMING("animal-taming", FlagTagRegister.ENTITY, FlagTagRegister.PLAYER),
    ANIMAL_UNMOUNTING("animal-unmounting", FlagTagRegister.ENTITY, FlagTagRegister.PLAYER),
    AXE_STRIP("strip-wood", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    BREAK_BLOCKS("break-blocks", FlagTagRegister.BLOCK, FlagTagRegister.PLAYER),
    CONTAINER_ACCESS("access-container", FlagTagRegister.BLOCK, FlagTagRegister.PLAYER),
    DRAGON_BLOCK_PROT("dragon-destruction", FlagTagRegister.ENVIRONMENT, FlagTagRegister.BLOCK),
    /* also prevents drop-xp currently */
    DROP_LOOT_ALL("drop-loot", FlagTagRegister.PLAYER_PREVENTION),
    /* also prevents drop-xp currently */
    DROP_LOOT_PLAYER("drop-loot-player", FlagTagRegister.PLAYER, FlagTagRegister.PLAYER_PREVENTION),
    ENDERMAN_GRIEFING("enderman-griefing", FlagTagRegister.PROTECTION),
    ENDERMAN_TELEPORT_FROM_REGION("enderman-tp-from", FlagTagRegister.PROTECTION),
    ENDER_CHEST_ACCESS("access-enderchest", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    ENTER_DIM("enter-dim", FlagTagRegister.PLAYER),
    EXECUTE_COMMAND("exec-command", FlagTagRegister.PLAYER),
    EXPLOSION_BLOCK("explosions-blocks", FlagTagRegister.PROTECTION),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-blocks", FlagTagRegister.PROTECTION),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-entities", FlagTagRegister.PROTECTION),
    EXPLOSION_ENTITY("explosions-entities", FlagTagRegister.PROTECTION),
    FALL_DAMAGE_ANIMALS("fall-damage-animals", FlagTagRegister.PROTECTION),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters", FlagTagRegister.PROTECTION),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers", FlagTagRegister.PROTECTION),
    FLUID_FLOW("fluid-flow", FlagTagRegister.ENVIRONMENT, FlagTagRegister.HIGH_FREQUENCY),
    HOE_TILL("till-farmland", FlagTagRegister.BLOCK, FlagTagRegister.PLAYER_PREVENTION),
    IGNITE_EXPLOSIVES("ignite-explosives", FlagTagRegister.BLOCK, FlagTagRegister.PLAYER_PREVENTION),
    FALL_DAMAGE("fall-damage", FlagTagRegister.PROTECTION, FlagTagRegister.ENVIRONMENT),
    FALL_DAMAGE_PLAYERS("fall-damage-players", FlagTagRegister.PLAYER, FlagTagRegister.PROTECTION),
    INVINCIBLE("invincible", FlagTagRegister.BENEFICIAL, FlagTagRegister.PLAYER),
    KEEP_XP("keep-xp", FlagTagRegister.BENEFICIAL, FlagTagRegister.PLAYER),
    KEEP_INV("keep-inv", FlagTagRegister.BENEFICIAL, FlagTagRegister.PLAYER),
    NO_HUNGER("no-hunger", FlagTagRegister.BENEFICIAL, FlagTagRegister.PLAYER),
    NO_KNOCKBACK("no-knockback", FlagTagRegister.BENEFICIAL, FlagTagRegister.PLAYER),
    ITEM_DROP("item-drop", FlagTagRegister.PLAYER_PREVENTION),
    ITEM_PICKUP("item-pickup", FlagTagRegister.PLAYER_PREVENTION),
    LAVA_FLOW("lava-flow", FlagTagRegister.ENVIRONMENT, FlagTagRegister.HIGH_FREQUENCY),
    LEVEL_FREEZE("level-freeze", FlagTagRegister.PLAYER),
    LIGHTNING_PROT("lightning", FlagTagRegister.ENVIRONMENT),
    NO_WALKER_FREEZE("walker-freeze", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    LEAF_DECAY("leaf-decay", FlagTagRegister.ENVIRONMENT),
    FIRE_TICK("fire-tick", FlagTagRegister.ENVIRONMENT),
    MELEE_ANIMALS("melee-animals", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    MELEE_MONSTERS("melee-monsters", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    MELEE_PLAYERS("melee-players", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    MELEE_VILLAGERS("melee-villagers", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    MELEE_WANDERING_TRADER("melee-wtrader", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    MOB_GRIEFING("mob-griefing", FlagTagRegister.ENVIRONMENT),
    NO_FLIGHT("no-flight", FlagTagRegister.PLAYER),
    NO_ITEM_DESPAWN("no-item-despawn", FlagTagRegister.PROTECTION, FlagTagRegister.ITEM, FlagTagRegister.ENVIRONMENT),
    NO_PVP("no-pvp", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    FIRE_BOW("fire-bow", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    NO_SIGN_EDIT("no-sign-edit", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    PLACE_BLOCKS("place-blocks", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    PLACE_FLUIDS("place-fluids", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    SCOOP_FLUIDS("scoop-fluids", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    SEND_MESSAGE("send-chat", FlagTagRegister.PLAYER),
    SET_SPAWN("set-spawn", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    SHOVEL_PATH("shovel-path", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    SHULKER_TELEPORT_FROM_REGION("shulker-tp-from", FlagTagRegister.ENTITY),
    SLEEP("sleep", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    SNOW_FALL("snow-fall", FlagTagRegister.BLOCK, FlagTagRegister.ENVIRONMENT),
    SNOW_MELTING("snow-melting", FlagTagRegister.BLOCK, FlagTagRegister.ENVIRONMENT),
    SPAWNING_ALL("spawning-all", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_ANIMAL("spawning-animal", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_GOLEM("spawning-golem", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_MONSTER("spawning-monster", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_SLIME("spawning-slime", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_TRADER("spawning-trader", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_VILLAGER("spawning-villager", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWNING_XP("spawning-xp", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    SPAWN_PORTAL("spawn-portal", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    TOOL_SECONDARY_USE("tools-secondary", FlagTagRegister.PLAYER_PREVENTION),
    TRAMPLE_FARMLAND("trample-farmland", FlagTagRegister.ENVIRONMENT, FlagTagRegister.BLOCK),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other", FlagTagRegister.BLOCK, FlagTagRegister.ENTITY),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    USE_BLOCKS("use-blocks", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    USE_BONEMEAL("use-bonemeal", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    USE_ELYTRA("use-elytra", FlagTagRegister.PLAYER),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    USE_ENDERPEARL_TO_REGION("enderpearl-to", FlagTagRegister.PLAYER, FlagTagRegister.BLOCK),
    USE_ENTITIES("use-entities", FlagTagRegister.PLAYER, FlagTagRegister.ENTITY),
    USE_ITEMS("use-items", FlagTagRegister.PLAYER_PREVENTION),
    USE_PORTAL("use-portal", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    USE_PORTAL_ANIMALS("use-portal-animals", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    USE_PORTAL_ITEMS("use-portal-items", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    USE_PORTAL_MINECARTS("use-portal-minecarts", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    USE_PORTAL_MONSTERS("use-portal-monsters", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    USE_PORTAL_PLAYERS("use-portal-players", FlagTagRegister.PLAYER),
    USE_PORTAL_VILLAGERS("use-portal-villagers", FlagTagRegister.ENVIRONMENT, FlagTagRegister.ENTITY),
    WATER_FLOW("water-flow", FlagTagRegister.ENVIRONMENT, FlagTagRegister.HIGH_FREQUENCY),
    WITHER_BLOCK_PROT("wither-destruction", FlagTagRegister.ENVIRONMENT, FlagTagRegister.BLOCK),
    XP_DROP_ALL("xp-drop-all", FlagTagRegister.PLAYER_PREVENTION),
    XP_DROP_MONSTER("xp-drop-monsters", FlagTagRegister.PLAYER_PREVENTION),
    XP_DROP_OTHER("xp-drop-other", FlagTagRegister.PLAYER_PREVENTION),
    XP_DROP_PLAYER("xp-drop-player", FlagTagRegister.PLAYER, FlagTagRegister.PLAYER_PREVENTION),
    XP_FREEZE("xp-freeze", FlagTagRegister.PLAYER),
    XP_PICKUP("xp-pickup", FlagTagRegister.PLAYER),
    ZOMBIE_DOOR_PROT("zombie-destruction", FlagTagRegister.ENVIRONMENT, FlagTagRegister.PROTECTION);

    public final String name;
    public final FlagType type;
    public final List<FlagTag> tags;

    RegionFlag(String name, FlagTag... tags) {
        this.name = name;
        this.type = FlagType.BOOLEAN_FLAG;
        this.tags = Arrays.asList(tags);
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
                .filter(flag -> flag.tags.contains(category))
                .collect(Collectors.toSet());
    }

    public static boolean hasPlayerCategory(IFlag flag) {
        Set<RegionFlag> flagsMatchingCategory = getFlagsMatchingCategory(FlagTagRegister.PLAYER);
        RegionFlag regionFlag = RegionFlag.fromId(flag.getName());
        return flagsMatchingCategory.contains(regionFlag);
    }

    public static boolean hasPlayerCategory(RegionFlag regionFlag) {
        return regionFlag.tags.contains(FlagTagRegister.PLAYER);
    }

    public static boolean hasCategory(RegionFlag regionFlag, FlagTag category) {
        return regionFlag.tags.contains(category);
    }

    public static boolean matchesCategory(RegionFlag regionFlag, Set<String> categories) {
        var flagCategories = regionFlag.tags.stream()
                .map(c -> c.tagRl().toString())
                .collect(Collectors.toSet());
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
        return this.is(FlagTagRegister.BENEFICIAL);
    }

    public boolean is(FlagTag flagTag) {
        return this.tags.contains(flagTag);
    }

    public boolean isPlayerFlag() {
        return this.is(FlagTagRegister.PLAYER);
    }
}