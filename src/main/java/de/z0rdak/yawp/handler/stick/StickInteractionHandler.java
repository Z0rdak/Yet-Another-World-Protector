package de.z0rdak.yawp.handler.stick;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.event.entity.player.AnvilRepairEvent;
import net.neoforged.neoforge.event.entity.player.PlayerInteractEvent;

import java.util.Objects;

import static de.z0rdak.yawp.util.StickUtil.*;
import static net.neoforged.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public class StickInteractionHandler {

    private StickInteractionHandler() {
    }

    @SubscribeEvent
    public static void onRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (!event.getLevel().isClientSide) {
            ItemStack involvedItemStack = event.getItemStack();
            if (!involvedItemStack.equals(ItemStack.EMPTY) && isVanillaStick(involvedItemStack)) {
                StickType stickType = getStickType(involvedItemStack);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    MarkerStickHandler.onMarkBlock(involvedItemStack, event);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onCycleMode(PlayerInteractEvent.RightClickItem event) {
        if (!event.getLevel().isClientSide) {
            ItemStack involvedItemStack = event.getItemStack();
            // is some valid mod stick
            if (!involvedItemStack.equals(ItemStack.EMPTY)
                    && hasNonNullTag(involvedItemStack)
                    && involvedItemStack.getTag().contains(STICK)) {
                HitResult blockLookingAt = event.getEntity().pick(20.0d, 0.0f, false);
                boolean targetIsAir;
                if (blockLookingAt.getType() == HitResult.Type.BLOCK) {
                    BlockPos blockpos = ((BlockHitResult) blockLookingAt).getBlockPos();
                    BlockState blockstate = event.getLevel().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                } else {
                    targetIsAir = blockLookingAt.getType() == HitResult.Type.MISS;
                }

                if (event.getEntity().isShiftKeyDown() && targetIsAir) {
                    StickType stickType = getStickType(involvedItemStack);
                    if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                        // Note: cycling mode is disabled for now because there is only one working area type
                        //MarkerStickHandler.onCycleRegionMarker(involvedItemStack);
                    }
                }
            }
        }
    }

    /**
     * Handles action when renaming mod sticks in an anvil.
     * This is used to create a mod stick or to define a region by renaming a valid RegionMarker stick.
     */
    public static void onStickRename(AnvilRepairEvent event) {
        Player player = event.getEntity();
        if (!player.getCommandSenderWorld().isClientSide) {
            ItemStack outputItem = event.getOutput();
            ItemStack inputItem = event.getLeft();
            ItemStack ingredientInput = event.getRight();
            boolean isInputAndOutputStick = ItemStack.matches(outputItem, Items.STICK.getDefaultInstance())
                    && ItemStack.matches(inputItem, Items.STICK.getDefaultInstance());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                onCreateStick(event);
            }
        }
    }

    /**
     * Edits the NBT data of the renamed stick to "transform" it to the corresponding mod stick.
     * @param event the event data from renaming the stick item
     */
    private static void onCreateStick(AnvilRepairEvent event) {
        Player player = event.getEntity();
        ItemStack outputItem = event.getOutput();
        ItemStack inputItem = event.getLeft();
        StickType type = StickType.of(outputItem.getHoverName().getString());
        if (type != StickType.UNKNOWN) {
            // split stack and only create one stick, also refund xp
            inputItem.setCount(outputItem.getCount() - 1);
            player.addItem(inputItem);
            event.setBreakChance(0.0f);
            player.giveExperienceLevels(1);
            initMarkerNbt(outputItem, type, event.getEntity().getCommandSenderWorld().dimension());
            player.getInventory().setChanged();
        }
    }
}
