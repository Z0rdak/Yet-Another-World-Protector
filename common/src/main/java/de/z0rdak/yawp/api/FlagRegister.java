package de.z0rdak.yawp.api;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.*;
import net.minecraft.ResourceLocationException;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class FlagRegister {

    private FlagRegister() {
    }

    private static final Map<ResourceLocation, Flag> flagRegister = new HashMap<>();

    private static ResourceLocation flagId(final String flagName) {
        return ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, flagName);
    }

    /**
     * Checks if the given flag ID matches the specified Flag.
     *
     * @param flagId The string representation of the flag ID.
     * @param flag   The flag to compare against.
     * @return True if the flag ID matches the flag's ResourceLocation, false otherwise.
     */
    public static boolean isSame(String flagId, Flag flag) {
        ResourceLocation left = ResourceLocation.tryParse(flagId);
        return left != null && left.equals(flag.id());
    }

    /**
     * Checks if two Flag instances represent the same flag.
     *
     * @param left  The first flag.
     * @param right The second flag.
     * @return True if both flags have the same ResourceLocation, false otherwise.
     */
    public static boolean isSame(Flag left, Flag right) {
        return isSame(left.id(), right);
    }

    /**
     * Checks if the given ResourceLocation matches the specified Flag.
     *
     * @param flagId The ResourceLocation of the flag.
     * @param flag   The flag to compare against.
     * @return True if the ResourceLocation matches the flag's ResourceLocation, false otherwise.
     */
    public static boolean isSame(ResourceLocation flagId, Flag flag) {
        return flagId.equals(flag.id());
    }

    /**
     * Registers a flag in the flag registry if it is not already registered.
     * The flag's resource location should follow the format {@code modId:flagId} to ensure uniqueness.
     *
     * @param flag The flag to register.
     * @return {@code true} if the flag was successfully registered, {@code false} if it was already registered.
     */
    private static boolean registerFlag(Flag flag) {
        if (isFlagRegistered(flag.id())) {
            return false;
        }
        flagRegister.put(flag.id(), flag);
        return true;
    }

    /**
     * Registers a flag in the internal flag registry using a {@link ResourceLocation} and {@link FlagMetaInfo}.
     * If a flag with the same resource location already exists, registration is skipped.
     *
     * @param flagRl       The unique resource location of the flag.
     * @param flagMetaInfo The metadata associated with the flag.
     * @return {@code true} if the flag was successfully registered, {@code false} if it was already registered.
     */
    public static boolean registerFlag(@NotNull ResourceLocation flagRl, @NotNull FlagMetaInfo flagMetaInfo) {
        if (flagRl.getNamespace().equalsIgnoreCase(Constants.MOD_ID)) {
            throw new IllegalArgumentException("You are not permitted to register flags with the YAWP namespace!");
        }
        return registerFlag(new Flag(flagRl, flagMetaInfo));
    }

    /**
     * Registers a flag in the internal flag registry using the modId of the registering mod and a flagId.
     * If a flag with the same resource location already exists, registration is skipped.
     *
     * @param modId        The namespace of the mod defining the flag.
     * @param flagId       The unique identifier of the flag within the mod's namespace.
     * @param flagMetaInfo The metadata associated with the flag.
     * @return {@code true} if the flag was successfully registered, {@code false} if it was already registered.
     */
    public static boolean registerFlag(@NotNull String modId, @NotNull String flagId, @NotNull FlagMetaInfo flagMetaInfo) {
        var rl = ResourceLocation.tryBuild(modId, flagId);
        if (rl == null) {
            return  false;
        }
        return registerFlag(rl, flagMetaInfo);
    }

    /**
     * Retrieves all registered flags that match a given frequency.
     *
     * @param frequency The frequency to filter flags by.
     * @return A set of flags that have the specified frequency.
     */
    public static Set<Flag> getFlagsByFrequency(FlagFrequency frequency) {
        return flagRegister.values().stream()
                .filter(flag -> flag.flagInfo().frequency() == frequency)
                .collect(Collectors.toSet());
    }

    /**
     * Checks if a flag is registered based on its ResourceLocation.
     *
     * @param rl The ResourceLocation of the flag.
     * @return True if the flag is registered, false otherwise.
     */
    public static boolean isFlagRegistered(ResourceLocation rl) {
        return flagRegister.containsKey(rl);
    }

    /**
     * Checks if a flag is registered based on its string identifier.
     *
     * @param flagIdentifier The string representation of the flag.
     * @return True if the flag is registered, false otherwise.
     */
    public static boolean isRegistered(String flagIdentifier) {
        try {
            ResourceLocation rl = ResourceLocation.tryParse(flagIdentifier);
            if (rl == null) {
                return false;
            }
            return isFlagRegistered(rl);
        } catch (ResourceLocationException rle) {
            return false;
        }
    }

    /**
     * Retrieves a Flag by its string identifier.
     *
     * @param flagIdentifier The string representation of the flag.
     * @return The corresponding Flag.
     * @throws IllegalArgumentException If the flag is not registered.
     */
    public static Flag byId(String flagIdentifier) throws IllegalArgumentException {
        if (isRegistered(flagIdentifier)) {
            ResourceLocation rl = ResourceLocation.tryParse(flagIdentifier);
            if (rl == null) {
                throw new IllegalArgumentException("Invalid region flag identifier supplied");
            }
            return flagRegister.get(rl);
        }
        throw new IllegalArgumentException("Invalid region flag identifier supplied");
    }

    /**
     * Retrieves an Optional containing the Flag if it exists.
     *
     * @param rl The ResourceLocation of the flag.
     * @return An Optional containing the flag if registered, otherwise empty.
     */
    public static Optional<Flag> getFlagOptional(ResourceLocation rl) {
        return isFlagRegistered(rl) ? Optional.of(flagRegister.get(rl)) : Optional.empty();
    }

    /**
     * Retrieves a Flag by its ResourceLocation, or null if not found.
     *
     * @param rl The ResourceLocation of the flag.
     * @return The corresponding Flag if registered, otherwise null.
     */
    @Nullable
    public static Flag getFlag(ResourceLocation rl) {
        return isFlagRegistered(rl) ? flagRegister.get(rl) : null;
    }

    /**
     * Gets a list of all registered flag names as strings.
     *
     * @return A list of flag names.
     */
    public static List<String> getFlagNames() {
        return flagRegister.keySet().stream()
                .map(ResourceLocation::toString)
                .collect(Collectors.toList());
    }

    /**
     * Retrieves a set of all registered Flags.
     *
     * @return A set containing all registered Flags.
     */
    public static Set<Flag> getFlags() {
        return new HashSet<>(flagRegister.values());
    }

    /**
     * Retrieves all flags that match a given category tag.
     *
     * @param tag The category tag to filter flags by.
     * @return A set of flags that match the given tag.
     */
    public static Set<Flag> getFlagsMatchingCategory(FlagTag tag) {
        return getFlags().stream()
                .filter(flag -> flag.flagInfo().tags().contains(tag))
                .collect(Collectors.toSet());
    }

    /**
     * Checks if a given flag has the PLAYER tag based on its identifier.
     *
     * @param flag The flag to check.
     * @return True if the flag has the PLAYER tag, false otherwise.
     */
    public static boolean hasPlayerTag(IFlag flag) {
        Set<Flag> flagsMatchingCategory = getFlagsMatchingCategory(FlagTag.PLAYER);
        Flag regionFlag = byId(flag.getName());
        return flagsMatchingCategory.contains(regionFlag);
    }

    /**
     * Checks if a given flag has the PLAYER tag.
     *
     * @param flag The flag to check.
     * @return True if the flag has the PLAYER tag, false otherwise.
     */
    public static boolean hasPlayerTag(Flag flag) {
        return flag.flagInfo().tags().contains(FlagTag.PLAYER);
    }

    /**
     * Determines if a flag belongs to a given set of category tags.
     *
     * @param flag The flag to check.
     * @param tags The set of category tags.
     * @return True if the flag has at least one of the given tags, false otherwise.
     */
    public static boolean matchesCategory(Flag flag, Set<String> tags) {
        Set<String> flagTags = flag.flagInfo().tags().stream()
                .map(c -> c.name)
                .collect(Collectors.toSet());
        return tags.stream().anyMatch(flagTags::contains);
    }

    public static final Flag PLAYER_BREED_ANIMAL = new Flag(flagId("player/breed_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag PLAYER_MOUNT = new Flag(flagId("player/mount"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.NEGLIGIBLE));
    public static final Flag PLAYER_TAME_ANIMAL = new Flag(flagId("player/tame_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.NEGLIGIBLE));
    public static final Flag PLAYER_UNMOUNTING = new Flag(flagId("player/unmount"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.NEGLIGIBLE));
    public static final Flag PLAYER_STRIP_WOOD = new Flag(flagId("player/strip_wood"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_BREAK_BLOCKS = new Flag(flagId("player/break_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_USE_CONTAINER = new Flag(flagId("player/use_container"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_GAIN_LOOT = new Flag(flagId("player/gain_loot"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag PLAYER_USE_CHEST_ACCESS = new Flag(flagId("player/use_enderchest"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_ENTER_LEVEL = new Flag(flagId("player/enter_level"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag PLAYER_USE_COMMANDS = new Flag(flagId("player/use_commands"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag PLAYER_TILL = new Flag(flagId("player/till_farmland"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_IGNITE_EXPLOSIVES = new Flag(flagId("player/ignite_explosives"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag PLAYER_HURT = new Flag(flagId("player/hurt"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NORMAL));
    public static final Flag PLAYER_DROP_ITEM = new Flag(flagId("player/drop_item"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_PICKUP_ITEM = new Flag(flagId("player/item_pickup"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_APPLY_KNOCKBACK = new Flag(flagId("player/apply_knockback"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_KNOCKBACK = new Flag(flagId("player/knockback"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_GAIN_LEVEL = new Flag(flagId("player/gain_level"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_WALKER_FREEZE = new Flag(flagId("player/walker_freeze"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLAYER_FALL_DAMAGE = new Flag(flagId("player/fall_damage"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_FIRE_BOW = new Flag(flagId("player/fire_bow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_MELEE_ANIMALS = new Flag(flagId("player/melee_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_MELEE_MONSTERS = new Flag(flagId("player/melee_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_MELEE_PLAYERS = new Flag(flagId("player/melee_players"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_MELEE_VILLAGERS = new Flag(flagId("player/melee_villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_MELEE_WANDERING_TRADER = new Flag(flagId("player/melee_wtrader"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NEGLIGIBLE));
    public static final Flag PLAYER_FLIGHT = new Flag(flagId("player/flight"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_PVP = new Flag(flagId("player/pvp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_EDIT_SIGNS = new Flag(flagId("player/edit_signs"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_PLACE_BLOCKS = new Flag(flagId("player/place_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLAYER_PLACE_FLUIDS = new Flag(flagId("player/place_fluids"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLAYER_SCOOP_FLUIDS = new Flag(flagId("player/scoop_fluids"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLAYER_CHAT = new Flag(flagId("player/chat"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_SET_SPAWN = new Flag(flagId("player/set_spawn"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_SHOVEL_PATH = new Flag(flagId("player/shovel_path"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_CREATE_PORTAL = new Flag(flagId("player/create_portal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_TOOL_SECONDARY = new Flag(flagId("player/tools_secondary"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ITEM), FlagFrequency.NORMAL));
    public static final Flag PLAYER_TRAMPLE_FARMLAND = new Flag(flagId("player/trample_farmland"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_USE_BLOCKS = new Flag(flagId("player/use_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLAYER_USE_BONEMEAL = new Flag(flagId("player/use_bonemeal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLAYER_USE_ELYTRA = new Flag(flagId("player/use_elytra"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_USE_ENDERPEARL = new Flag(flagId("player/use_enderpearl"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_ENDERPEARL_AWAY = new Flag(flagId("player/enderpearl_away"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_INTERACT = new Flag(flagId("player/interact"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag PLAYER_USE_ITEMS = new Flag(flagId("player/use_items"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ITEM), FlagFrequency.NORMAL));
    public static final Flag PLAYER_SLEEP = new Flag(flagId("player/sleep"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLAYER_ENTER_PORTAL = new Flag(flagId("player/enter_portal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_KEEP_XP = new Flag(flagId("player/keep_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_KEEP_INV = new Flag(flagId("player/keep_inv"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_APPLY_HUNGER = new Flag(flagId("player/apply_hunger"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_DROP_XP = new Flag(flagId("player/drop_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag PLAYER_GAIN_XP = new Flag(flagId("player/gain_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag PLAYER_PICKUP_XP = new Flag(flagId("player/pickup_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag DRAGON_BLOCK_PROT = new Flag(flagId("env/dragon_destruction"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag DROP_LOOT_ALL = new Flag(flagId("entity/drop_loot"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag ENDERMAN_GRIEFING = new Flag(flagId("griefing/enderman"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag ENDERMAN_TELEPORT = new Flag(flagId("mob/enderman_tp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag EXPLOSION_BLOCK = new Flag(flagId("explosion/blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag EXPLOSION_ENTITY = new Flag(flagId("explosion/entities"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag EXPLOSION_CREEPER_BLOCK = new Flag(flagId("griefing/creeper_explosion_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag EXPLOSION_CREEPER_ENTITY = new Flag(flagId("griefing/creeper_explosion_entities"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag TRAMPLE_FARMLAND = new Flag(flagId("griefing/trample_farmland"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag FALL_DAMAGE = new Flag(flagId("entity/fall_damage"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NORMAL));
    public static final Flag FALL_DAMAGE_ANIMALS = new Flag(flagId("entity/fall_damage_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.LOW));
    public static final Flag FALL_DAMAGE_MONSTERS = new Flag(flagId("entity/fall_damage_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.LOW));
    public static final Flag SHULKER_TELEPORT = new Flag(flagId("entity/shulker_tp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag FALL_DAMAGE_VILLAGERS = new Flag(flagId("entity/fall_damage_villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NEGLIGIBLE));
    public static final Flag LAVA_FLOW = new Flag(flagId("env/lava_flow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.HIGH_FREQUENCY, FlagTag.ENVIRONMENT), FlagFrequency.VERY_HIGH));
    public static final Flag LIGHTNING_PROT = new Flag(flagId("env/lightning"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag FLUID_FLOW = new Flag(flagId("env/fluid_flow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.HIGH_FREQUENCY, FlagTag.ENVIRONMENT), FlagFrequency.VERY_HIGH));
    public static final Flag LEAF_DECAY = new Flag(flagId("env/leaf_decay"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag FIRE_TICK = new Flag(flagId("env/fire_tick"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag NO_ITEM_DESPAWN = new Flag(flagId("item/despawn"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ITEM, FlagTag.PROTECTION), FlagFrequency.NORMAL));

    public static final Flag MOB_GRIEFING = new Flag(flagId("mob/griefing"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag SNOW_FALL = new Flag(flagId("env/snow_fall"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag SNOW_MELTING = new Flag(flagId("env/snow_melting"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag SPAWNING_ALL = new Flag(flagId("spawning/all"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.TICK));
    public static final Flag SPAWNING_ANIMAL = new Flag(flagId("spawning/animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWNING_GOLEM = new Flag(flagId("spawning/golems"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SPAWNING_MONSTER = new Flag(flagId("spawning/monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWNING_SLIME = new Flag(flagId("spawning/slimes"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWNING_TRADER = new Flag(flagId("spawning/traders"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SPAWNING_VILLAGER = new Flag(flagId("spawning/villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SPAWNING_XP = new Flag(flagId("spawning/xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL = new Flag(flagId("env/enter_portal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_ANIMALS = new Flag(flagId("env/enter_portal_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_ITEMS = new Flag(flagId("env/enter_portal_items"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_MINECARTS = new Flag(flagId("env/enter_portal_minecarts"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_MONSTERS = new Flag(flagId("env/enter_portal_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_VILLAGERS = new Flag(flagId("env/enter_portal_villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag WATER_FLOW = new Flag(flagId("env/water_flow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.HIGH_FREQUENCY, FlagTag.ENVIRONMENT), FlagFrequency.VERY_HIGH));
    public static final Flag WITHER_BLOCK_PROT = new Flag(flagId("griefing/wither_destruction"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag XP_DROP_ALL = new Flag(flagId("env/drop_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag XP_DROP_MONSTER = new Flag(flagId("entity/drop_xp_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag ZOMBIE_DOOR_PROT = new Flag(flagId("griefing/break_doors"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));

    static {
        registerFlag(PLAYER_BREED_ANIMAL);
        registerFlag(PLAYER_MOUNT);
        registerFlag(PLAYER_TAME_ANIMAL);
        registerFlag(PLAYER_UNMOUNTING);
        registerFlag(PLAYER_STRIP_WOOD);
        registerFlag(PLAYER_BREAK_BLOCKS);
        registerFlag(PLAYER_USE_CONTAINER);
        registerFlag(PLAYER_GAIN_LOOT);
        registerFlag(PLAYER_USE_CHEST_ACCESS);
        registerFlag(PLAYER_ENTER_LEVEL);
        registerFlag(PLAYER_USE_COMMANDS);
        registerFlag(PLAYER_TILL);
        registerFlag(PLAYER_IGNITE_EXPLOSIVES);
        registerFlag(PLAYER_HURT);
        registerFlag(PLAYER_DROP_ITEM);
        registerFlag(PLAYER_PICKUP_ITEM);
        registerFlag(PLAYER_APPLY_KNOCKBACK);
        registerFlag(PLAYER_KNOCKBACK);
        registerFlag(PLAYER_GAIN_LEVEL);
        registerFlag(PLAYER_WALKER_FREEZE);
        registerFlag(PLAYER_FALL_DAMAGE);
        registerFlag(PLAYER_FIRE_BOW);
        registerFlag(PLAYER_MELEE_ANIMALS);
        registerFlag(PLAYER_MELEE_MONSTERS);
        registerFlag(PLAYER_MELEE_PLAYERS);
        registerFlag(PLAYER_MELEE_VILLAGERS);
        registerFlag(PLAYER_MELEE_WANDERING_TRADER);
        registerFlag(PLAYER_FLIGHT);
        registerFlag(PLAYER_PVP);
        registerFlag(PLAYER_EDIT_SIGNS);
        registerFlag(PLAYER_PLACE_BLOCKS);
        registerFlag(PLAYER_PLACE_FLUIDS);
        registerFlag(PLAYER_SCOOP_FLUIDS);
        registerFlag(PLAYER_CHAT);
        registerFlag(PLAYER_SET_SPAWN);
        registerFlag(PLAYER_SHOVEL_PATH);
        registerFlag(PLAYER_CREATE_PORTAL);
        registerFlag(PLAYER_TOOL_SECONDARY);
        registerFlag(PLAYER_TRAMPLE_FARMLAND);
        registerFlag(PLAYER_USE_BLOCKS);
        registerFlag(PLAYER_USE_BONEMEAL);
        registerFlag(PLAYER_USE_ELYTRA);
        registerFlag(PLAYER_USE_ENDERPEARL);
        registerFlag(PLAYER_ENDERPEARL_AWAY);
        registerFlag(PLAYER_INTERACT);
        registerFlag(PLAYER_USE_ITEMS);
        registerFlag(PLAYER_SLEEP);
        registerFlag(PLAYER_ENTER_PORTAL);
        registerFlag(PLAYER_KEEP_XP);
        registerFlag(PLAYER_KEEP_INV);
        registerFlag(PLAYER_APPLY_HUNGER);
        registerFlag(PLAYER_DROP_XP);
        registerFlag(PLAYER_GAIN_XP);
        registerFlag(PLAYER_PICKUP_XP);
        registerFlag(DRAGON_BLOCK_PROT);
        registerFlag(DROP_LOOT_ALL);
        registerFlag(ENDERMAN_GRIEFING);
        registerFlag(ENDERMAN_TELEPORT);
        registerFlag(EXPLOSION_BLOCK);
        registerFlag(EXPLOSION_CREEPER_BLOCK);
        registerFlag(EXPLOSION_CREEPER_ENTITY);
        registerFlag(EXPLOSION_ENTITY);
        registerFlag(FALL_DAMAGE);
        registerFlag(FALL_DAMAGE_ANIMALS);
        registerFlag(FALL_DAMAGE_MONSTERS);
        registerFlag(FALL_DAMAGE_VILLAGERS);
        registerFlag(FLUID_FLOW);
        registerFlag(LAVA_FLOW);
        registerFlag(LIGHTNING_PROT);
        registerFlag(LEAF_DECAY);
        registerFlag(FIRE_TICK);
        registerFlag(MOB_GRIEFING);
        registerFlag(NO_ITEM_DESPAWN);
        registerFlag(SHULKER_TELEPORT);
        registerFlag(SNOW_FALL);
        registerFlag(SNOW_MELTING);
        registerFlag(SPAWNING_ALL);
        registerFlag(SPAWNING_ANIMAL);
        registerFlag(SPAWNING_GOLEM);
        registerFlag(SPAWNING_MONSTER);
        registerFlag(SPAWNING_SLIME);
        registerFlag(SPAWNING_TRADER);
        registerFlag(SPAWNING_VILLAGER);
        registerFlag(SPAWNING_XP);
        registerFlag(TRAMPLE_FARMLAND);
        registerFlag(USE_PORTAL);
        registerFlag(USE_PORTAL_ANIMALS);
        registerFlag(USE_PORTAL_ITEMS);
        registerFlag(USE_PORTAL_MINECARTS);
        registerFlag(USE_PORTAL_MONSTERS);
        registerFlag(USE_PORTAL_VILLAGERS);
        registerFlag(WATER_FLOW);
        registerFlag(WITHER_BLOCK_PROT);
        registerFlag(XP_DROP_ALL);
        registerFlag(XP_DROP_MONSTER);
        registerFlag(ZOMBIE_DOOR_PROT);
    }

}
