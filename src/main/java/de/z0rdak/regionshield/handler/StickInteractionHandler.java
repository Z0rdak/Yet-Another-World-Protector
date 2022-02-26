package de.z0rdak.regionshield.handler;

import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.util.StickType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.event.entity.player.AnvilRepairEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.regionshield.util.StickUtil.*;

@Mod.EventBusSubscriber(modid = RegionShield.MODID)
public class StickInteractionHandler {

    @SubscribeEvent
    public static void onRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (!event.getWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            ItemStack usedItem = player.getItemInHand(player.getUsedItemHand());
            if (isStick(usedItem)) {
                StickType stickType = getStickType(usedItem);
                boolean isShiftPressed = player.isShiftKeyDown();
                BlockRayTraceResult blockRayTraceResult = event.getHitVec();
                BlockPos pos = blockRayTraceResult.getBlockPos();
                RayTraceResult.Type traceResultType = blockRayTraceResult.getType();

                switch (stickType) {
                    case REGION_STICK:

                        break;
                    case FLAG_STICK:

                        break;
                    case MARKER:
                        // add marking type for nbt (area type)
                        handleRegionMarking(event);
                        break;
                    default:
                        break;
                }
            }
        }


        // TODO: check block and handle stick action accordingly

        // TODO: rendering and charge use needs to be implemented in stickitem mixin
    }

    private static void handleRegionMarking(PlayerInteractEvent.RightClickBlock event) {

    }


    @SubscribeEvent
    public static void onRightClickAir(PlayerInteractEvent.RightClickEmpty event) {
        // TODO: retrieve stick used

        // TODO: cycle modes according to sticks
    }

    @SubscribeEvent
    public static void onLeftClickBlock(PlayerInteractEvent.LeftClickBlock event) {

    }

    public static void onAttackPlayer(AttackEntityEvent event) {
        // TODO: handle add players with region stick
    }


    /**
     * MAYBE: Rename only one stick and split stack
     *
     * @param event
     */
    @SubscribeEvent
    public static void onStickRename(AnvilRepairEvent event) {
        PlayerEntity player = event.getPlayer();
        if (!player.getCommandSenderWorld().isClientSide) {
            ItemStack outputItem = event.getItemResult();
            ItemStack inputItem = event.getItemInput();
            ItemStack ingredientInput = event.getIngredientInput();
            boolean isInputAndOutputStick = ItemStack.isSame(outputItem, Items.STICK.getDefaultInstance())
                    && ItemStack.isSame(inputItem, Items.STICK.getDefaultInstance());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                String stickName = outputItem.getDisplayName().getString();
                stickName = stickName.substring(1, stickName.length() - 1);
                StickType type = StickType.of(stickName);
                if (type != StickType.UNKNOWN) {
                    inputItem.setCount(outputItem.getCount() - 1);
                    player.addItem(inputItem);
                    // TODO: Send network packet to force inventory sync
                    outputItem.setCount(1);
                    updateStickTag(outputItem, type);
                    setStickNameAndTooltip(outputItem, type);
                    applyEnchantmentGlint(outputItem);
                }
            }
        }
    }


}
