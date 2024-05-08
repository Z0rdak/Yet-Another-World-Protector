package de.z0rdak.yawp.util;

import com.mojang.serialization.DataResult;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.component.DataComponentTypes;
import net.minecraft.component.type.LoreComponent;
import net.minecraft.component.type.NbtComponent;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.text.Text;
import net.minecraft.text.TextColor;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

import static net.minecraft.util.Formatting.*;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String MARKED_BLOCKS = "blocks";
    public static final String VALID_AREA = "valid";
    public static final String AREA_TYPE = "type";
    public static final String DIM = "dim";
    public static final String TP_POS = "tp_pos";
    public static final String IS_TP_SET = "is_tp_set";
    public static final String STICK_TYPE = "stick_type";
    public static final String STICK_ID = "stick-id";
    public static final String STICK = "stick";

    public static void applyEnchantmentGlint(ItemStack item) {
        item.set(DataComponentTypes.ENCHANTMENT_GLINT_OVERRIDE, true);
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param type  stick type to create
     * @param dim   dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, StickType type, RegistryKey<World> dim) {
        NbtCompound stickTag = getStickTag(stick);
        NbtCompound itemTag = stickTag != null ? stickTag : new NbtCompound();
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            NbtCompound compoundNBT = new MarkerStick(dim).serializeNBT();
            itemTag.put(STICK, compoundNBT);
            // TODO: get custom data first, then replace the stick tag
            stick.set(DataComponentTypes.CUSTOM_DATA, ).setNbt(itemTag);
        }
    }

    public static ItemStack initMarkerNbt(ItemStack stack, StickType type, RegistryKey<World> dim) {
        stack.setCount(1);
        initStickTag(stack, type, dim);
        setStickName(stack, type);
        setToolTip(stack);
        applyEnchantmentGlint(stack);
        return stack;
    }

    public static boolean isVanillaStick(ItemStack itemStack) {
        return ItemStack.areItemsEqual(itemStack, Items.STICK.getDefaultStack());
    }

    public static AbstractStick getStick(ItemStack stick) throws StickException {
        if (hasStickTag(stick)) {
            NbtCompound stickNbt = getStickTag(stick);
            StickType type = StickType.of(stickNbt.getString(STICK_TYPE));
            switch (type) {
                case MARKER:
                    return new MarkerStick(stickNbt);
                case UNKNOWN:
                default:
                    throw new StickException("Unknown stick type: '" + type + "'!");
            }
        }
        throw new StickException("Invalid or missing NBT data for Stick '" + stick.toHoverableText().getString() + "'!");
    }

    public static StickType getStickType(ItemStack stick) {
        NbtCompound stickTag = getStickTag(stick);
        if (stickTag != null) {
            return StickType.of(stickTag.getString(STICK_TYPE));
        }
        return StickType.UNKNOWN;
    }

    @Nullable
    public static NbtCompound getStickTag(ItemStack stick) {
        if (hasStickTag(stick)) {
            NbtComponent nbtComponent = stick.get(DataComponentTypes.CUSTOM_DATA);
            DataResult<Optional<NbtCompound>> optionalDataResult = nbtComponent.get(NbtCompound.CODEC.optionalFieldOf(STICK));
            // optionalDataResult.ifSuccess(e -> e.ifPresent(ex -> ex.get()))
        }
        return null;
    }

    public static void setStickName(ItemStack stick, StickType type) {
        String displayName = "";
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            MarkerStick marker = new MarkerStick(getStickTag(stick));
            String validFlag = marker.isValidArea() ? (GREEN + "*" + GOLD) : "";
            displayName = GOLD + type.stickName + " (" + marker.getAreaType().areaType + "" + validFlag + ")";
        }
        stick.set(DataComponentTypes.CUSTOM_NAME, Text.literal(displayName));
    }

    public static void setToolTip(ItemStack stack) {
        var color1 = TextColor.parse("#ff4d4d").getOrThrow().getRgb();
        var color2 = TextColor.parse("#808080").getOrThrow().getRgb();
        var loreList = new ArrayList<Text>();
        loreList.add(Text.translatable("help.tooltip.stick.marker.simple.1").withColor(color1));
        loreList.add(Text.translatable("help.tooltip.stick.marker.simple.2").withColor(color1));
        loreList.add(Text.translatable("help.tooltip.stick.marker.simple.3").formatted(ITALIC).withColor(color2));
        loreList.add(Text.translatable("help.tooltip.stick.marker.simple.4").formatted(ITALIC).withColor(color2));
        stack.set(DataComponentTypes.LORE, new LoreComponent(loreList));
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.get(DataComponentTypes.CUSTOM_DATA) != null;
    }

    public static boolean hasStickTag(ItemStack stack) {
        if (hasNonNullTag(stack)) {
            return stack.get(DataComponentTypes.CUSTOM_DATA).contains(STICK);
        }
        return false;
    }
}
