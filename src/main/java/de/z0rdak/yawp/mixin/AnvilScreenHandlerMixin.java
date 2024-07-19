package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.stick.MarkerStickHandler;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.screen.AnvilScreenHandler;
import net.minecraft.screen.ScreenHandler;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(AnvilScreenHandler.class)
public abstract class AnvilScreenHandlerMixin {

    @Inject(method = "onTakeOutput", at = @At("HEAD"), cancellable = true, allow = 1)
    private void onTakeOutput(PlayerEntity player, ItemStack outputItem, CallbackInfo ci) {
        if (!player.getWorld().isClient) {
            // Retrieve the input and output items from the anvil menu
            ItemStack inputItem = ((ScreenHandler) (Object) this).getSlot(0).getStack();
            ItemStack ingredientInput = ((ScreenHandler) (Object) this).getSlot(1).getStack();

            boolean isInputAndOutputStick = ItemStack.areItemsEqual(outputItem, Items.STICK.getDefaultStack())
                    && ItemStack.areItemsEqual(inputItem, Items.STICK.getDefaultStack());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                // TODO: Alpha1 - RegionMarker disabled
                // StickType type = StickType.of(outputItem.getName().getString());
                // if (type != StickType.UNKNOWN) {
                //     MarkerStickHandler.onCreateStick(player, inputItem, outputItem, type);
                //     player.getInventory().markDirty();
                //     ci.cancel();
                // }
            }
        }
    }
}
