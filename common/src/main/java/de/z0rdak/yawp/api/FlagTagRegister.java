package de.z0rdak.yawp.api;

import de.z0rdak.yawp.core.flag.FlagTag;
import net.minecraft.resources.Identifier;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public final class FlagTagRegister {

    private static final Map<Identifier, FlagTag> REGISTRY = new ConcurrentHashMap<>();

    /**
     * Flags that evaluate per-player context or directly
     * affect player actions.
     * <p>Examples: {@code BREAK_BLOCKS}, {@code PLACE_BLOCKS},
     * {@code USE_ITEM}, etc. </p>
     */
    public static final FlagTag PLAYER = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "player"),
            "Flags that evaluate per-player context or directly affect player actions. Examples: BREAK_BLOCKS, PLACE_BLOCKS, USE_ITEM, etc."
    ));

    /**
     * Flags granting advantages or permissions to players.
     * A flag is “beneficial” if denying it would feel wrong
     * from a player perspective.
     * <p>Examples: {@code INVINCIBLE}, {@code KEEP_INV},
     * {@code KEEP_XP}, etc.</p>
     * Usually combined with {@link #PLAYER}.
     */
    public static final FlagTag BENEFICIAL = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "beneficial"),
            "Flags granting advantages or permissions to players. Denying these feels wrong from a player perspective. Usually combined with PLAYER. Examples: INVINCIBLE, KEEP_INV, KEEP_XP."
    ));

    /**
     * Flags that actively block or prevent a player’s attempted action.
     * <p>Examples: {@code BREAK_BLOCKS}, {@code PLACE_BLOCKS},
     * {@code INTERACT_ENTITY}, {@code USE_ITEM}.</p>
     * Always implies {@link #PLAYER}.
     */
    public static final FlagTag PLAYER_PREVENTION = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "prevention"),
            "Flags that actively block or prevent a player’s attempted action. Always implies PLAYER. Examples: BREAK_BLOCKS, PLACE_BLOCKS, USE_ITEM."
    ));

    /**
     * Flags concerning block-based events or world state.
     * <p>Examples: {@code FIRE_SPREAD}, {@code TNT_EXPLODE},
     * {@code BLOCK_BREAK}, etc.</p>
     */
    public static final FlagTag BLOCK = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "block"),
            "Flags concerning block-based events or world state. Examples: FIRE_SPREAD, TNT_EXPLODE, BLOCK_BREAK."
    ));

    /**
     * Flags tied to item actions or mechanics.
     * <p>Examples: {@code USE_ITEM}, {@code DROP_ITEM},
     * {@code PICKUP_ITEM}.</p>
     * Often combined with {@link #PLAYER}.
     */
    public static final FlagTag ITEM = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "item"),
            "Flags tied to item actions or mechanics. Examples: USE_ITEM, DROP_ITEM, PICKUP_ITEM. Often combined with PLAYER."
    ));

    /**
     * Flags involving entities as targets/source.
     * <p>Examples: {@code INTERACT_ENTITY}.</p>
     */
    public static final FlagTag ENTITY = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "entity"),
            "Flags involving entities as targets or sources. Examples: INTERACT_ENTITY."
    ));

    /**
     * Flags controlling world-level effects or
     * non-entity mechanics.
     * <p>Examples: {@code FIRE_TICK}, {@code LEAF_DECAY},
     * {@code FLUID_FLOW}, {@code ICE_MELT}.</p>
     */
    public static final FlagTag ENVIRONMENT = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "environment"),
            "Flags controlling world-level effects or non-entity mechanics. Examples: FIRE_TICK, LEAF_DECAY, FLUID_FLOW, ICE_MELT."
    ));

    /**
     * Flags checked repeatedly per n-ticks,
     * usually for movement or environment updates.
     * <p>Examples: {@code MOVE}, {@code FIRE_TICK}, {@code FLUID_FLOW}.</p>
     */
    public static final FlagTag HIGH_FREQUENCY = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "high-frequency"),
            "Flags checked repeatedly per n-ticks, usually for movement or environment updates. Examples: MOVE, FIRE_TICK, FLUID_FLOW."
    ));

    /**
     * Flags defining preventive or defensive behavior
     * rather than granting permissions.
     * <p>Examples: {@code BLOCK_EXPLODE}, {@code griefing}, etc.</p>
     */
    public static final FlagTag PROTECTION = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "protection"),
            "Flags defining preventive or defensive behavior rather than granting permissions. Examples: BLOCK_EXPLODE, griefing."
    ));

    /**
     * Flags defining preventive or defensive behavior
     * rather than granting permissions.
     * <p>Examples: {@code BLOCK_EXPLODE}, {@code griefing}, etc.</p>
     */
    public static final FlagTag SPAWNING = register(new FlagTag(
            Identifier.fromNamespaceAndPath("yawp", "spawning"),
            "Flags related to entity Spawning and its prevention. Examples: spawning/monsters"
    ));

    /** Registers a new FlagTag in the central registry. */
    public static FlagTag register(FlagTag tag) {
        REGISTRY.put(tag.tagRl(), tag);
        return tag;
    }

    /** Retrieves a FlagTag by its Identifier. */
    public static FlagTag from(Identifier rl) throws IllegalArgumentException {
        FlagTag tag = REGISTRY.get(rl);
        if (tag == null) throw new IllegalArgumentException("Invalid flag tag: " + rl);
        return tag;
    }

    /** Returns all registered FlagTags. */
    public static Collection<FlagTag> getAll() {
        return Collections.unmodifiableCollection(REGISTRY.values());
    }
}
