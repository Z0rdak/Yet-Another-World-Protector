package de.z0rdak.yawp.core.flag;

import net.minecraft.network.chat.TranslatableComponent;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum RegionFlag {

    BREAK_BLOCKS(new ConditionFlag("break_blocks", false)),
    SCOOP_FLUIDS(new ConditionFlag("scoop_fluids", false)),
    BREAK_ENTITIES(new ConditionFlag("break_entities", false)),
    PLACE_BLOCKS(new ConditionFlag("place_blocks", false)),
    PLACE_FLUIDS(new ConditionFlag("place_fluids", false)),
    ENTITY_PLACE(new ConditionFlag("entity-place", false)),
    EXPLOSION_ENTITY(new ConditionFlag("explosions-entities", false)),
    EXPLOSION_BLOCK(new ConditionFlag("explosions-blocks", false)),
    EXPLOSION_CREEPER_BLOCK(new ConditionFlag("creeper-explosion-entities", false)),
    EXPLOSION_CREEPER_ENTITY(new ConditionFlag("creeper-explosion-blocks", false)),
    EXPLOSION_OTHER_BLOCKS(new ConditionFlag("other-explosion-entities", false)),
    EXPLOSION_OTHER_ENTITY(new ConditionFlag("other-explosion-blocks", false)),
    IGNITE_EXPLOSIVES(new ConditionFlag("ignite-explosives", false)),
    TOOL_SECONDARY_USE(new ConditionFlag("tools-secondary", false)),
    AXE_STRIP(new ConditionFlag("strip-wood", false)),
    HOE_TILL(new ConditionFlag("till-farmland", false)),
    SHOVEL_PATH(new ConditionFlag("shovel-path", false)),
    TRAMPLE_FARMLAND(new ConditionFlag("trample-farmland", false)),
    TRAMPLE_FARMLAND_PLAYER(new ConditionFlag("trample-farmland-player", false)),
    TRAMPLE_FARMLAND_OTHER(new ConditionFlag("trample-farmland-other", false)),
    DRAGON_BLOCK_PROT(new ConditionFlag("dragon-destruction", false)),
    WITHER_BLOCK_PROT(new ConditionFlag("wither-destruction", false)),
    ZOMBIE_DOOR_PROT(new ConditionFlag("zombie-destruction", false)),
    LIGHTNING_PROT(new ConditionFlag("lightning", false)),
    //
    ANIMAL_TAMING(new ConditionFlag("animal-taming", false)),
    ANIMAL_BREEDING(new ConditionFlag("animal-breeding", false)),
    ANIMAL_MOUNTING(new ConditionFlag("animal-mounting", false)),
    ANIMAL_UNMOUNTING(new ConditionFlag("animal-unmounting", false)),
    SPAWNING_MONSTERS(new ConditionFlag("spawning-monsters", false)),
    SPAWNING_GOLEM(new ConditionFlag("spawning-irongolem", false)),
    SPAWNING_ANIMAL(new ConditionFlag("spawning-animal", false)),
    SPAWNING_ALL(new ConditionFlag("spawning-all", false)),
    SPAWNING_XP(new ConditionFlag("spawning-xp", false)),
    USE(new ConditionFlag("use", false)), // Buttons, Doors, Lever, e, false), falsetc
    USE_BONEMEAL(new ConditionFlag("use-bonemeal", false)),
    CONTAINER_ACCESS(new ConditionFlag("access-container", false)),
    ENDER_CHEST_ACCESS(new ConditionFlag("access-enderchest", false)),
    USE_ENDERPEARL_FROM_REGION(new ConditionFlag("enderpearl-from", false)),
    USE_ENDERPEARL_TO_REGION(new ConditionFlag("enderpearl-to", false)),
    ENDERMAN_TELEPORT_TO_REGION(new ConditionFlag("enderman-teleport-to", false)),
    ENDERMAN_TELEPORT_FROM_REGION(new ConditionFlag("enderman-teleport-from", false)),
    SHULKER_TELEPORT_TO_REGION(new ConditionFlag("shulker-teleport-to", false)),
    SHULKER_TELEPORT_FROM_REGION(new ConditionFlag("shulker-teleport-from", false)),
    ITEM_DROP(new ConditionFlag("item-drop", false)),
    // unrelated: mobs pickup logic => MobEntity#livingTick
    ITEM_PICKUP(new ConditionFlag("item-pickup", false)),
    LOOT_DROP(new ConditionFlag("loot-drop", false)),
    XP_DROP_ALL(new ConditionFlag("xp-drop-all", false)), // also includes blocks (furnace for example)
    XP_DROP_MONSTER(new ConditionFlag("xp-drop-monsters", false)), // only hostile mobs
    XP_DROP_OTHER(new ConditionFlag("xp-drop-other", false)), // non-hostile: animals, villagers,...
    XP_PICKUP(new ConditionFlag("xp-pickup", false)),
    LEVEL_FREEZE(new ConditionFlag("level-freeze", false)),
    XP_FREEZE(new ConditionFlag("xp-freeze", false)),
    ATTACK_PLAYERS(new ConditionFlag("attack-players", false)),
    ATTACK_ANIMALS(new ConditionFlag("attack-animals", false)),
    ATTACK_MONSTERS(new ConditionFlag("attack-monsters", false)),
    ATTACK_VILLAGERS(new ConditionFlag("attack-villagers", false)),
    ATTACK_WANDERING_TRADER(new ConditionFlag("attack-wtrader", false)),
    INVINCIBLE(new ConditionFlag("invincible", false)),
    FALL_DAMAGE(new ConditionFlag("fall-damage", false)),
    FALL_DAMAGE_VILLAGERS(new ConditionFlag("fall-damage-villagers", false)),
    FALL_DAMAGE_MONSTERS(new ConditionFlag("fall-damage-monsters", false)),
    FALL_DAMAGE_ANIMALS(new ConditionFlag("fall-damage-animals", false)),
    FALL_DAMAGE_PLAYERS(new ConditionFlag("fall-damage-players", false)),
    SEND_MESSAGE(new ConditionFlag("send-chat", false)),
    EXECUTE_COMMAND(new ConditionFlag("exec-command", false)),
    SET_SPAWN(new ConditionFlag("set-spawn", false)),
    // RESET_SPAWN(new ConditionFlag("reset-spawn"), // not working
    SLEEP(new ConditionFlag("sleep", false)),
    USE_PORTAL(new ConditionFlag("use-portal", false)),
    //TRAVEL_TO_DIM(new ConditionFlag("travel-to-dim", false)),
    //TRAVEL_FROM_DIM(new ConditionFlag("travel-from-dim", false)),
    USE_PORTAL_PLAYERS(new ConditionFlag("use-portal-players", false)),
    USE_PORTAL_ITEMS(new ConditionFlag("use-portal-items", false)),
    USE_PORTAL_ANIMALS(new ConditionFlag("use-portal-animals", false)),
    USE_PORTAL_MONSTERS(new ConditionFlag("use-portal-monsters", false)),
    USE_PORTAL_VILLAGERS(new ConditionFlag("use-portal-villagers", false)),
    USE_PORTAL_MINECARTS(new ConditionFlag("use-portal-minecarts", false)),
    SPAWN_PORTAL(new ConditionFlag("spawn-portal", false));
    //PLAYER_SPAWN_PORTAL(new ConditionFlag("player-spawn-portal", false));


    public final String flagname;
    public final IFlag flag;
    public final String langKey;
    // public final boolean defaultValue;
    public final Function<List<String>, TranslatableComponent> denyMessageBuilder;

    RegionFlag(IFlag flag) {
        this.flag = flag;
        this.flagname = flag.getFlagName();
        this.langKey = "rs.flag.deny.message." + flag.getFlagName();
        this.denyMessageBuilder = (args) -> new TranslatableComponent(this.langKey, args == null ? new ArrayList<>() : args);
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
    public static List<String> getFlags() {
        return Arrays.stream(RegionFlag.values())
                .map(RegionFlag::toString)
                .collect(Collectors.toList());
    }

    public static Optional<RegionFlag> fromString(String flagIdentifier){
        return Arrays.stream(values())
                .filter(flag -> flag.flagname.equals(flagIdentifier))
                .findFirst();
    }
}
