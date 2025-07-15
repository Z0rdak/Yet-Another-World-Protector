package de.z0rdak.yawp.api;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.*;
import net.minecraft.ResourceLocationException;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class FlagRegister {

    private FlagRegister() {}


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
        ResourceLocation left = ResourceLocation.parse(flagId);
        return left.equals(flag.id());
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
     * Registers a flag in the internal flag registry if it is not already registered.
     * The flag's resource location should follow the format {@code modId:flagId} to ensure uniqueness.
     *
     * @param flag The flag to register.
     * @return {@code true} if the flag was successfully registered, {@code false} if it was already registered.
     */
    public static boolean registerFlag(Flag flag) {
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
     * @param flagRl The unique resource location of the flag.
     * @param flagMetaInfo The metadata associated with the flag.
     * @return {@code true} if the flag was successfully registered, {@code false} if it was already registered.
     */
    public static boolean registerFlag(ResourceLocation flagRl, FlagMetaInfo flagMetaInfo) {
        return registerFlag(new Flag(flagRl, flagMetaInfo));
    }

    /**
     * Registers a flag in the internal flag registry using the modId of the registering mod and a flagId.
     * If a flag with the same resource location already exists, registration is skipped.
     *
     * @param modId The namespace of the mod defining the flag.
     * @param flagId The unique identifier of the flag within the mod's namespace.
     * @param flagMetaInfo The metadata associated with the flag.
     * @return {@code true} if the flag was successfully registered, {@code false} if it was already registered.
     */
    public static boolean registerFlag(String modId, String flagId, FlagMetaInfo flagMetaInfo) {
        var rl = ResourceLocation.fromNamespaceAndPath(modId, flagId);
        return registerFlag(new Flag(rl, flagMetaInfo));
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
            ResourceLocation rl = ResourceLocation.parse(flagIdentifier);
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
            return flagRegister.get(ResourceLocation.parse(flagIdentifier));
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


    /* Flags defined by YAWP */
    public static final Flag ANIMAL_BREEDING = new Flag(flagId("animal_breeding"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag ANIMAL_MOUNTING = new Flag(flagId("animal_mounting"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.NEGLIGIBLE));
    public static final Flag ANIMAL_TAMING = new Flag(flagId("animal_taming"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.NEGLIGIBLE));
    public static final Flag ANIMAL_UNMOUNTING = new Flag(flagId("animal_unmounting"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY, FlagTag.PLAYER), FlagFrequency.NEGLIGIBLE));
    public static final Flag AXE_STRIP = new Flag(flagId("strip_wood"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag BREAK_BLOCKS = new Flag(flagId("break_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag CONTAINER_ACCESS = new Flag(flagId("access_container"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag DRAGON_BLOCK_PROT = new Flag(flagId("dragon_destruction"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag DROP_LOOT_ALL = new Flag(flagId("drop_loot"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag DROP_LOOT_PLAYER = new Flag(flagId("drop_loot_player"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag ENDERMAN_GRIEFING = new Flag(flagId("enderman_griefing"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag ENDERMAN_TELEPORT_FROM_REGION = new Flag(flagId("enderman_tp_from"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.LOW));
    public static final Flag ENDER_CHEST_ACCESS = new Flag(flagId("access_enderchest"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag ENTER_DIM = new Flag(flagId("enter_dim"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag EXECUTE_COMMAND = new Flag(flagId("exec_command"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag EXPLOSION_BLOCK = new Flag(flagId("explosions_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag EXPLOSION_ENTITY = new Flag(flagId("explosions_entities"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag EXPLOSION_CREEPER_BLOCK = new Flag(flagId("creeper_explosion_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag EXPLOSION_CREEPER_ENTITY = new Flag(flagId("creeper_explosion_entities"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag FALL_DAMAGE = new Flag(flagId("fall_damage"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NORMAL));
    public static final Flag FALL_DAMAGE_ANIMALS = new Flag(flagId("fall_damage_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.LOW));
    public static final Flag FALL_DAMAGE_MONSTERS = new Flag(flagId("fall_damage_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.LOW));
    public static final Flag FALL_DAMAGE_PLAYERS = new Flag(flagId("fall_damage_players"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NORMAL));
    public static final Flag FALL_DAMAGE_VILLAGERS = new Flag(flagId("fall_damage_villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NEGLIGIBLE));
    public static final Flag FLUID_FLOW = new Flag(flagId("fluid_flow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.HIGH_FREQUENCY, FlagTag.ENVIRONMENT), FlagFrequency.VERY_HIGH));
    public static final Flag HOE_TILL = new Flag(flagId("till_farmland"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag IGNITE_EXPLOSIVES = new Flag(flagId("ignite_explosives"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.PLAYER), FlagFrequency.LOW));
    public static final Flag INVINCIBLE = new Flag(flagId("invincible"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION), FlagFrequency.NORMAL));
    public static final Flag ITEM_DROP = new Flag(flagId("item_drop"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag ITEM_PICKUP = new Flag(flagId("item_pickup"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag KNOCKBACK_PLAYERS = new Flag(flagId("knockback_players"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag LAVA_FLOW = new Flag(flagId("lava_flow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.HIGH_FREQUENCY, FlagTag.ENVIRONMENT), FlagFrequency.VERY_HIGH));
    public static final Flag LEVEL_FREEZE = new Flag(flagId("level_freeze"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag LIGHTNING_PROT = new Flag(flagId("lightning"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag NO_WALKER_FREEZE = new Flag(flagId("walker_freeze"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag LEAF_DECAY = new Flag(flagId("leaf_decay"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag FIRE_TICK = new Flag(flagId("fire_tick"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag FIRE_BOW = new Flag(flagId("fire_bow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag MELEE_ANIMALS = new Flag(flagId("melee_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag MELEE_MONSTERS = new Flag(flagId("melee_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag MELEE_PLAYERS = new Flag(flagId("melee_players"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag MELEE_VILLAGERS = new Flag(flagId("melee_villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag MELEE_WANDERING_TRADER = new Flag(flagId("melee_wtrader"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NEGLIGIBLE));
    public static final Flag MOB_GRIEFING = new Flag(flagId("mob_griefing"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag NO_FLIGHT = new Flag(flagId("no_flight"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag NO_ITEM_DESPAWN = new Flag(flagId("no_item_despawn"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ITEM, FlagTag.PROTECTION), FlagFrequency.NORMAL));
    public static final Flag NO_PVP = new Flag(flagId("no_pvp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag NO_SIGN_EDIT = new Flag(flagId("no_sign_edit"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag PLACE_BLOCKS = new Flag(flagId("place_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag PLACE_FLUIDS = new Flag(flagId("place_fluids"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag SCOOP_FLUIDS = new Flag(flagId("scoop_fluids"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag SEND_MESSAGE = new Flag(flagId("send_chat"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag SET_SPAWN = new Flag(flagId("set_spawn"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag SHOVEL_PATH = new Flag(flagId("shovel_path"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag SHULKER_TELEPORT_FROM_REGION = new Flag(flagId("shulker_tp_from"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SLEEP = new Flag(flagId("sleep"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag SNOW_FALL = new Flag(flagId("snow_fall"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag SNOW_MELTING = new Flag(flagId("snow_melting"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.ENVIRONMENT), FlagFrequency.TICK));
    public static final Flag SPAWNING_ALL = new Flag(flagId("spawning_all"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.TICK));
    public static final Flag SPAWNING_ANIMAL = new Flag(flagId("spawning_animal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWNING_GOLEM = new Flag(flagId("spawning_golem"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SPAWNING_MONSTER = new Flag(flagId("spawning_monster"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWNING_SLIME = new Flag(flagId("spawning_slime"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWNING_TRADER = new Flag(flagId("spawning_trader"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SPAWNING_VILLAGER = new Flag(flagId("spawning_villager"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag SPAWNING_XP = new Flag(flagId("spawning_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag SPAWN_PORTAL = new Flag(flagId("spawn_portal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag TOOL_SECONDARY_USE = new Flag(flagId("tools_secondary"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ITEM), FlagFrequency.NORMAL));
    public static final Flag TRAMPLE_FARMLAND = new Flag(flagId("trample_farmland"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag TRAMPLE_FARMLAND_OTHER = new Flag(flagId("trample_farmland_other"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.BLOCK, FlagTag.ENTITY), FlagFrequency.LOW));
    public static final Flag TRAMPLE_FARMLAND_PLAYER = new Flag(flagId("trample_farmland_player"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag USE_BLOCKS = new Flag(flagId("use_blocks"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag USE_BONEMEAL = new Flag(flagId("use_bonemeal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag USE_ELYTRA = new Flag(flagId("use_elytra"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag USE_ENDERPEARL_FROM_REGION = new Flag(flagId("enderpearl_from"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag USE_ENDERPEARL_TO_REGION = new Flag(flagId("enderpearl_to"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.BLOCK), FlagFrequency.LOW));
    public static final Flag USE_ENTITIES = new Flag(flagId("use_entities"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_ITEMS = new Flag(flagId("use_items"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER, FlagTag.ITEM), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL = new Flag(flagId("use_portal"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_ANIMALS = new Flag(flagId("use_portal_animals"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_ITEMS = new Flag(flagId("use_portal_items"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_MINECARTS = new Flag(flagId("use_portal_minecarts"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_MONSTERS = new Flag(flagId("use_portal_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_PLAYERS = new Flag(flagId("use_portal_players"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag USE_PORTAL_VILLAGERS = new Flag(flagId("use_portal_villagers"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.NORMAL));
    public static final Flag WATER_FLOW = new Flag(flagId("water_flow"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.HIGH_FREQUENCY, FlagTag.ENVIRONMENT), FlagFrequency.VERY_HIGH));
    public static final Flag WITHER_BLOCK_PROT = new Flag(flagId("wither_destruction"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.BLOCK), FlagFrequency.NORMAL));
    public static final Flag KEEP_XP = new Flag(flagId("keep_xp"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag KEEP_INV = new Flag(flagId("keep_inv"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag NO_HUNGER = new Flag(flagId("no_hunger"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PROTECTION, FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag XP_DROP_ALL = new Flag(flagId("xp_drop_all"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag XP_DROP_MONSTER = new Flag(flagId("xp_drop_monsters"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag XP_DROP_OTHER = new Flag(flagId("xp_drop_other"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag XP_DROP_PLAYER = new Flag(flagId("xp_drop_player"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(), FlagFrequency.NORMAL));
    public static final Flag XP_FREEZE = new Flag(flagId("xp_freeze"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag XP_PICKUP = new Flag(flagId("xp_pickup"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.PLAYER), FlagFrequency.NORMAL));
    public static final Flag ZOMBIE_DOOR_PROT = new Flag(flagId("zombie_destruction"),
            new FlagMetaInfo(FlagType.BOOLEAN_FLAG, Set.of(FlagTag.ENVIRONMENT, FlagTag.ENTITY), FlagFrequency.LOW));

    static {
        registerFlag(ANIMAL_BREEDING);
        registerFlag(ANIMAL_MOUNTING);
        registerFlag(ANIMAL_TAMING);
        registerFlag(ANIMAL_UNMOUNTING);
        registerFlag(AXE_STRIP);
        registerFlag(BREAK_BLOCKS);
        registerFlag(CONTAINER_ACCESS);
        registerFlag(DRAGON_BLOCK_PROT);
        registerFlag(DROP_LOOT_ALL);
        registerFlag(DROP_LOOT_PLAYER);
        registerFlag(ENDERMAN_GRIEFING);
        registerFlag(ENDERMAN_TELEPORT_FROM_REGION);
        registerFlag(ENDER_CHEST_ACCESS);
        registerFlag(ENTER_DIM);
        registerFlag(EXECUTE_COMMAND);
        registerFlag(EXPLOSION_BLOCK);
        registerFlag(EXPLOSION_CREEPER_BLOCK);
        registerFlag(EXPLOSION_CREEPER_ENTITY);
        registerFlag(EXPLOSION_ENTITY);
        registerFlag(FALL_DAMAGE);
        registerFlag(FALL_DAMAGE_ANIMALS);
        registerFlag(FALL_DAMAGE_MONSTERS);
        registerFlag(FALL_DAMAGE_PLAYERS);
        registerFlag(FALL_DAMAGE_VILLAGERS);
        registerFlag(FLUID_FLOW);
        registerFlag(HOE_TILL);
        registerFlag(IGNITE_EXPLOSIVES);
        registerFlag(INVINCIBLE);
        registerFlag(ITEM_DROP);
        registerFlag(ITEM_PICKUP);
        registerFlag(KNOCKBACK_PLAYERS);
        registerFlag(LAVA_FLOW);
        registerFlag(LEVEL_FREEZE);
        registerFlag(LIGHTNING_PROT);
        registerFlag(NO_WALKER_FREEZE);
        registerFlag(LEAF_DECAY);
        registerFlag(FIRE_TICK);
        registerFlag(MELEE_ANIMALS);
        registerFlag(MELEE_MONSTERS);
        registerFlag(MELEE_PLAYERS);
        registerFlag(MELEE_VILLAGERS);
        registerFlag(MELEE_WANDERING_TRADER);
        registerFlag(MOB_GRIEFING);
        registerFlag(NO_FLIGHT);
        registerFlag(NO_ITEM_DESPAWN);
        registerFlag(NO_PVP);
        registerFlag(NO_SIGN_EDIT);
        registerFlag(PLACE_BLOCKS);
        registerFlag(PLACE_FLUIDS);
        registerFlag(SCOOP_FLUIDS);
        registerFlag(SEND_MESSAGE);
        registerFlag(SET_SPAWN);
        registerFlag(SHOVEL_PATH);
        registerFlag(SHULKER_TELEPORT_FROM_REGION);
        registerFlag(SLEEP);
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
        registerFlag(SPAWN_PORTAL);
        registerFlag(TOOL_SECONDARY_USE);
        registerFlag(TRAMPLE_FARMLAND);
        registerFlag(TRAMPLE_FARMLAND_OTHER);
        registerFlag(TRAMPLE_FARMLAND_PLAYER);
        registerFlag(USE_BLOCKS);
        registerFlag(USE_BONEMEAL);
        registerFlag(USE_ELYTRA);
        registerFlag(USE_ENDERPEARL_FROM_REGION);
        registerFlag(USE_ENDERPEARL_TO_REGION);
        registerFlag(USE_ENTITIES);
        registerFlag(USE_ITEMS);
        registerFlag(USE_PORTAL);
        registerFlag(USE_PORTAL_ANIMALS);
        registerFlag(USE_PORTAL_ITEMS);
        registerFlag(USE_PORTAL_MINECARTS);
        registerFlag(USE_PORTAL_MONSTERS);
        registerFlag(USE_PORTAL_PLAYERS);
        registerFlag(USE_PORTAL_VILLAGERS);
        registerFlag(WATER_FLOW);
        registerFlag(WITHER_BLOCK_PROT);
        registerFlag(KEEP_XP);
        registerFlag(KEEP_INV);
        registerFlag(NO_HUNGER);
        registerFlag(XP_DROP_ALL);
        registerFlag(XP_DROP_MONSTER);
        registerFlag(XP_DROP_OTHER);
        registerFlag(XP_DROP_PLAYER);
        registerFlag(XP_FREEZE);
        registerFlag(XP_PICKUP);
        registerFlag(ZOMBIE_DOOR_PROT);
    }

}
