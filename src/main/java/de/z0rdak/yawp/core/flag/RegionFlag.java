package de.z0rdak.yawp.core.flag;

import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum RegionFlag {

    BREAK_BLOCKS(new ConditionFlag("break_blocks", false)),
    PLACE_BLOCKS(new ConditionFlag("place_blocks", false));
    /*
    ENTITY_PLACE("entity-place"), // TODO: needs testing
    //
    EXPLOSION_ENTITY("explosions-entities"),
    EXPLOSION_BLOCK("explosions-blocks"),
    EXPLOSION_CREEPER_BLOCK("creeper-explosion-entities"),
    EXPLOSION_CREEPER_ENTITY("creeper-explosion-blocks"),
    EXPLOSION_OTHER_BLOCKS("other-explosion-entities"),
    EXPLOSION_OTHER_ENTITY("other-explosion-blocks"),
    IGNITE_EXPLOSIVES("ignite-explosives"),
    //
    TOOL_SECONDARY_USE("tools-secondary"),
    AXE_STRIP("strip-wood"),
    HOE_TILL("till-farmland"),
    SHOVEL_PATH("shovel-path"),
    //
    TRAMPLE_FARMLAND("trample-farmland"),
    TRAMPLE_FARMLAND_PLAYER("trample-farmland-player"),
    TRAMPLE_FARMLAND_OTHER("trample-farmland-other"),
    //
    //PISTON_PUSH("piston-push"),
    //PISTON_PULL("piston-pull"),
    //
    //FLUID_FLOWING("fluid-flowing"),
    //WATER_FLOWING("water-flowing"),
    //LAVA_FLOWING("lava-flowing"),
    //
    DRAGON_BLOCK_PROT("dragon-destruction"),
    WITHER_BLOCK_PROT("wither-destruction"),
    ZOMBIE_DOOR_PROT("zombie-destruction"),
    LIGHTNING_PROT("lightning"),
    //
    ANIMAL_TAMING("animal-taming"),
    ANIMAL_BREEDING("animal-breeding"),
    ANIMAL_MOUNTING("animal-mounting"),
    ANIMAL_UNMOUNTING("animal-unmounting"), // FIXME: Minecraft vanilla bug fixed in 21w03a
    //
    SPAWNING_MONSTERS("spawning-monsters"),
    SPAWNING_GOLEM("spawning-irongolem"),
    SPAWNING_ANIMAL("spawning-animal"),
    SPAWNING_ALL("spawning-all"),
    SPAWNING_XP("spawning-xp"),
    //
    USE("use"), // Buttons, Doors, Lever, etc
    USE_BONEMEAL("use-bonemeal"),
    CONTAINER_ACCESS("access-container"),
    ENDER_CHEST_ACCESS("access-enderchest"),
    USE_ENDERPEARL_FROM_REGION("enderpearl-from"),
    USE_ENDERPEARL_TO_REGION("enderpearl-to"),
    //
    ENDERMAN_TELEPORT_TO_REGION("enderman-teleport-to"),
    ENDERMAN_TELEPORT_FROM_REGION("enderman-teleport-from"),
    SHULKER_TELEPORT_TO_REGION("shulker-teleport-to"),
    SHULKER_TELEPORT_FROM_REGION("shulker-teleport-from"),
    //
    ITEM_DROP("item-drop"),
    ITEM_PICKUP("item-pickup"),
    // unrelated: mobs pickup logic => MobEntity#livingTick
    LOOT_DROP("loot-drop"),
    XP_DROP_ALL("xp-drop-all"), // also includes blocks (furnace for example)
    XP_DROP_MONSTER("xp-drop-monsters"), // only hostile mobs
    XP_DROP_OTHER("xp-drop-other"), // non-hostile: animals, villagers,...
    XP_PICKUP("xp-pickup"),
    LEVEL_FREEZE("level-freeze"),
    XP_FREEZE("xp-freeze"),
    //
    ATTACK_PLAYERS("attack-players"),
    ATTACK_ANIMALS("attack-animals"),
    ATTACK_MONSTERS("attack-monsters"),
    ATTACK_VILLAGERS("attack-villagers"),
    //
    INVINCIBLE("invincible"),
    //
    FALL_DAMAGE("fall-damage"),
    FALL_DAMAGE_VILLAGERS("fall-damage-villagers"),
    FALL_DAMAGE_MONSTERS("fall-damage-monsters"),
    FALL_DAMAGE_ANIMALS("fall-damage-animals"),
    FALL_DAMAGE_PLAYERS("fall-damage-players"),
    //
    SEND_MESSAGE("send-chat"),
    EXECUTE_COMMAND("exec-command"),
    SET_SPAWN("set-spawn"),
    // RESET_SPAWN("reset-spawn"), // not working
    SLEEP("sleep"),
    //
    // ELYTRA_FLY("elytra-fly"),
    // USE_FIREWORK("use-firework"),
    //
    USE_PORTAL("use-portal"),
    USE_PORTAL_PLAYERS("use-portal-players"),
    USE_PORTAL_ITEMS("use-portal-items"),
    USE_PORTAL_ANIMALS("use-portal-animals"),
    USE_PORTAL_MONSTERS("use-portal-monsters"),
    USE_PORTAL_VILLAGERS("use-portal-villagers"),
    USE_PORTAL_MINECARTS("use-portal-minecarts"),
    SPAWN_PORTAL("spawn-portal");
    */

    public final String flagname;
    public final IFlag flag;
    public final String langKey;
    // public final boolean defaultValue;
    public final Function<List<String>, IFormattableTextComponent> denyMessageBuilder;

    RegionFlag(IFlag flag) {
        this.flag = flag;
        this.flagname = flag.getFlagName();
        this.langKey = "rs.flag.deny.message." + flag.getFlagName();
        this.denyMessageBuilder = (args) -> new TranslationTextComponent(this.langKey, args == null ? new ArrayList<>() : args);
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
